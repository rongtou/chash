%% @author rong
%% @doc 
-module(hash_ring_test).

-export([start/0]).

start() ->
    test(1000, 5),
    test(10000, 5),
    test(100000, 5),
    test(1000000, 5),
    test(10000000, 5),
    ok.

update_counter(Node) ->
    case erlang:get(Node) of
        undefined ->
            erlang:put(Node, 1);
        Count ->
            erlang:put(Node, Count+1)
    end.

test(TestNum, NodeNum) ->
    Nodes = [erlang:list_to_binary(lists:concat(["n", I])) || I <- lists:seq(1, NodeNum)],
    Ring = hash_ring:new(Nodes),
    {Time, _Value} = timer:tc(fun() ->
        lists:foreach(fun(I) -> 
            Node = hash_ring:get_node(integer_to_list(I), Ring),
            update_counter(Node)
        end, lists:seq(1, TestNum))
    end),
    io:format("calc ~w data on ~w nodes time cost: ~w~n", [TestNum, NodeNum, Time]),
    io:format("distribution:~n~p~n~n", [[erlang:get(Node)||Node<-Nodes]]).
