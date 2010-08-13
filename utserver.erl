-module(utserver).

-export([address/1, open/1, open/2, close/1, send/2]).
-export([listener/2]).

-define(DEFAULT_PORT, 7778).

-define(UDP_SOCKET_OPTIONS, []).

-record(utconnection, {
	  pid,
	  socket,
	  address,
	  port
	 }).

address(X) -> X#utconnection.address.

open(Address) ->
    open(Address, ?DEFAULT_PORT).

open(Address, Port) ->
    Pid = spawn_link(?MODULE, listener, [self(), pending:new()]),
    {ok, Socket} = gen_udp:open(0, ?UDP_SOCKET_OPTIONS),
    ok = gen_udp:controlling_process(Socket, Pid),
    #utconnection{pid = Pid, socket = Socket, address = Address, port = Port}.


close(#utconnection{socket = Socket}) ->
    gen_udp:close(Socket).


send(Query, #utconnection{socket = S, address = A, port = P}) ->
    gen_udp:send(S, A, P, packet:make(Query)).


listener(Pid, Pending) ->
    receive
	{udp, _Socket, _IP, _InPortNo, Packet} ->
	    case packet:to_pairs(Packet) of
		{ok, {ID, N, [{"echo", Query} | Pairs]}} ->
		    Pending1 = pending:add(Pending, ID, N, Pairs),
		    case pending:is_complete(Pending1, ID) of
			true ->
			    {Msg, Pending2} = pending:take(Pending1, ID),
			    R = packet:build_reply(list_to_atom(Query), Msg),
			    Pid ! {utserver, R},
			    listener(Pid, Pending2);
			false ->
			    listener(Pid, Pending1)
		    end;
		{ok, [{"echo", Query} | Pairs]} ->
		    Reply = packet:build_reply(list_to_atom(Query), Pairs),
		    Pid ! {utserver, Reply},
		    listener(Pid, Pending);
		{incomplete, {ID, N, Pairs}} ->
		    NewPending = pending:add(Pending, ID, N, Pairs),
		    listener(Pid, NewPending);
		{error, Reason} ->
		    io:format("Error when building reply packet: ~p\nPacket (~p bytes):\n~p\n\n", [Reason, byte_size(Packet), Packet]),
		    listener(Pid, Pending)
	    end
    end.
