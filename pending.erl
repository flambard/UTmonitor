-module(pending).

-export([new/0, add/4, is_complete/2, take/2]).

%%%
%%% Pending = [{ID, Packets}]
%%%
%%% ID = int()
%%% Packets = [{N, Packet}]
%%%
%%% N = int()
%%% Packet = [{Key, Value}]
%%%
%%% Key = string()
%%% Value = string()
%%%

%%%
%%% add(Pending, ID, N, Packet) -> {complete, {Message, NewPending}} | {incomplete, NewPending}
%%%


new() -> [].


add(Pending, ID, N, Packet) ->
    case lists:keysearch(ID, 1, Pending) of
	false ->
	    [{ID, [{N, Packet}]} | Pending];
	{value, {ID, PreviousPackets}} ->
	    Packets = lists:keymerge(1, [{N, Packet}], PreviousPackets),
	    lists:keyreplace(ID, 1, Pending, {ID, Packets})
    end.

is_complete(Pending, ID) ->
    case lists:keysearch(ID, 1, Pending) of
	false -> false;
	{value, {ID, Packets}} -> ascending_keys(1, Packets)
    end.

ascending_keys(Pos, Tuples) ->
    ascending_keys(Pos, Tuples, 1).

ascending_keys(_Pos, [], _N) ->
    true;
ascending_keys(Pos, [Tuple | Tuples], N) ->
    N =:= element(Pos, Tuple) andalso ascending_keys(Pos, Tuples, N + 1).

take(Pending, ID) ->
    case lists:keytake(ID, 1, Pending) of
	false -> {[], Pending};
	{value, {ID, Packets}, SmallerPending} ->
	    Msg = lists:foldl(fun ({_N, P}, Acc) -> P ++ Acc end, [], Packets),
	    {Msg, SmallerPending}
    end.

