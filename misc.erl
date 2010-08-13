-module(misc).

-export([pairs/1, pairs_reversed/1, tokenize_binary/2, tokenize_binary_reversed/2, binary_foldl/3]).


pairs(List) ->
    lists:reverse(pairs_reversed(List)).

pairs_reversed(List) ->
    pairs_iter(List, []).

pairs_iter([Key, Value | Rest], Acc) ->
    pairs_iter(Rest, [{Key, Value} | Acc]);
pairs_iter([Last], Acc) ->
    [{Last, undefined} | Acc];
pairs_iter([], Acc) ->
    Acc.


tokenize_binary(Binary, Tokens) ->
    lists:reverse(tokenize_binary_reversed(Binary, Tokens)).

tokenize_binary_reversed(Binary, Tokens) ->
    case binary_foldl(fun (Byte, {BinAcc, ListAcc}) ->
			      case {lists:member(Byte, Tokens), BinAcc} of
				  {true, <<>>} -> {<<>>, ListAcc};
				  {true,    _} -> {<<>>, [BinAcc | ListAcc]};
				  {false,   _} -> {<<BinAcc/binary, Byte>>, ListAcc}
			      end
		      end,
		      {<<>>, []},
		      Binary)
	of
	{<<>>, Bins} -> Bins;
	{Last, Bins} -> [Last | Bins]
    end.


binary_foldl(Fun, Initial, <<Byte, Rest/binary>>) ->
    binary_foldl(Fun, Fun(Byte, Initial), Rest);
binary_foldl(_Fun, Initial, <<>>) ->
    Initial.
