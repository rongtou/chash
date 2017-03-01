%% @author rong
%% @doc consistent hashing using ketama
-module(hash_ring).

-export([new/1, add_nodes/2, remove_node/2, get_node/2]).

-define(VIR_NODE_NUM, 160).
-define(VIR_NODE_REPS, (?VIR_NODE_NUM div 4)).

new(Nodes) ->
    add_nodes(Nodes, gb_trees:empty()).

add_nodes(Nodes, Ring) ->
    UpdateFunc = fun(Pos, Node, Ring0) ->
        gb_trees:insert(Pos, Node, Ring0)
    end,
    lists:foldl(fun(Node, Ring0) ->
        update_virtual_nodes(Node, Ring0, ?VIR_NODE_REPS, UpdateFunc)
    end,  Ring, Nodes).

remove_node(Node, Ring) ->
    UpdateFunc = fun(Pos, _Node, Ring0) ->
        gb_trees:delete(Pos, Ring0)
    end,
    update_virtual_nodes(Node, Ring, ?VIR_NODE_REPS, UpdateFunc).

update_virtual_nodes(_Node, Ring, 0, _UpdateFunc) ->
    Ring;
update_virtual_nodes(Node, Ring0, Count, UpdateFunc) ->
    Digest = erlang:md5(<<Node/binary, "#", Count>>),
    Ring = update_ring(Digest, Node, Ring0, UpdateFunc),
    update_virtual_nodes(Node, Ring, Count-1, UpdateFunc).

update_ring(<<>>, _Node, Ring, _Fun) ->
    Ring;
update_ring(<<A,B,C,D,R/binary>>, Node, Ring0, UpdateFunc) ->
    Pos = (D bsl 24) bor (C bsl 16) bor (B bsl 8) bor A,
    Ring = UpdateFunc(Pos, Node, Ring0),
    update_ring(R, Node, Ring, UpdateFunc).

get_node(Key, Ring) ->
    HashKey = hash_key(Key),
    Iter = gb_trees:iterator_from(HashKey, Ring),
    case gb_trees:next(Iter) of
        {_, Node, _} -> Node;
        none -> element(2, gb_trees:smallest(Ring))
    end.

hash_key(Key) ->
    <<A,B,C,D,_/binary>> = erlang:md5(Key),
    (D bsl 24) bor (C bsl 16) bor (B bsl 8) bor A.

% gbtrees
% iterator_from(S, {_, T}) ->
%     iterator_1_from(S, T).

% iterator_1_from(S, T) ->
%     iterator_from(S, T, []).

% iterator_from(S, {K, _, _, T}, As) when K < S ->
%     iterator_from(S, T, As);
% iterator_from(_, {_, _, nil, _} = T, As) ->
%     [T | As];
% iterator_from(S, {_, _, L, _} = T, As) ->
%     iterator_from(S, L, [T | As]);
% iterator_from(_, nil, As) ->
%     As.

% dict:to_list(lists:foldl(fun(I, Dict) -> 
%     Node = chash:get_node(integer_to_list(I), Ring), 
%     dict:update_counter(Node, 1, Dict) 
% end, dict:new(), lists:seq(1, 100))).

