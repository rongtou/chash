%% @author rong
%% @doc consistent hashing using ketama
-module(hash_ring).

-export([new/1, get_node/2]).

-define(VIR_NODE_NUM, 160).

new(Nodes) ->
    lists:foldl(fun(Node, Ring0) ->
        insert_virtual_nodes(Node, Ring0, ?VIR_NODE_NUM div 4)
    end,  gb_trees:empty(), Nodes).

insert_virtual_nodes(_Node, Ring, 0) ->
    Ring;
insert_virtual_nodes(Node, Ring0, Count) ->
    Digest = erlang:md5(<<Node/binary, "#", Count>>),
    Ring = calc_pos_and_insert(Digest, Node, Ring0),
    insert_virtual_nodes(Node, Ring, Count-1).
    
calc_pos_and_insert(<<>>, _Node, Ring) ->
    Ring;
calc_pos_and_insert(<<A,B,C,D,R/binary>>, Node, Ring0) ->
    Pos = (D bsl 24) bor (C bsl 16) bor (B bsl 8) bor A,
    Ring = gb_trees:insert(Pos, Node, Ring0),
    calc_pos_and_insert(R, Node, Ring).

get_node(Key, Ring) ->
    HashKey = hash_key(Key),
    Iter = iterator_from(HashKey, Ring),
    case gb_trees:next(Iter) of
        {_, Node, _} -> Node;
        none -> element(2, gb_trees:smallest(Ring))
    end.

hash_key(Key) ->
    <<A,B,C,D,_/binary>> = erlang:md5(Key),
    (D bsl 24) bor (C bsl 16) bor (B bsl 8) bor A.

% gbtrees
iterator_from(S, {_, T}) ->
    iterator_1_from(S, T).

iterator_1_from(S, T) ->
    iterator_from(S, T, []).

iterator_from(S, {K, _, _, T}, As) when K < S ->
    iterator_from(S, T, As);
iterator_from(_, {_, _, nil, _} = T, As) ->
    [T | As];
iterator_from(S, {_, _, L, _} = T, As) ->
    iterator_from(S, L, [T | As]);
iterator_from(_, nil, As) ->
    As.

% dict:to_list(lists:foldl(fun(I, Dict) -> 
%     Node = chash:get_node(integer_to_list(I), Ring), 
%     dict:update_counter(Node, 1, Dict) 
% end, dict:new(), lists:seq(1, 100))).

