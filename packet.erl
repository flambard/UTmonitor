-module(packet).

-include("reply.hrl").

-export([make/1, to_pairs/1, build_reply/2]).


make(Q) ->
    String = atom_to_list(Q),
    ["\\", String, "\\\\echo\\", String].


to_pairs(Packet) ->
    Tokens = string:tokens(Packet, "\\"),
    case misc:pairs_reversed(Tokens) of
	[{"final", _}, {"queryid", QID}, {"echo", Query} | Pairs] ->
	    {ID, N} = read_query_id(QID),
	    {ok, {ID, N, [{"echo", Query} | Pairs]}};
	[{"final", _}, {"echo", Query} | Pairs] ->
	    {ok, [{"echo", Query} | Pairs]};
	[{"queryid", QID}, {"echo", Query} | Pairs] ->
	    {ID, N} = read_query_id(QID),
	    {ok, {ID, N, [{"echo", Query} | Pairs]}};
        [{"queryid", QID} | Pairs] ->
            {ID, N} = read_query_id(QID),
            {incomplete, {ID, N, Pairs}};
        [_ | _] ->
            {error, malformed_packet}
    end.


read_query_id(QID) ->
    {ID, N} = lists:splitwith(fun (C) -> [C] =:= "." end, QID),
    {ID, list_to_integer(N)}.


build_reply(Query, Keylist) ->
    lists:foldl(accumulator(Query), initial_value(Query), Keylist).


accumulator(basic) ->
    fun (_KV, {error, Reason}) -> {error, Reason};
	({Key, Value}, Acc) ->
	    case Key of
		"gamename"  -> Acc#basic{gamename = Value};
		"gamever"   -> Acc#basic{gamever = Value};
		"minnetver" -> Acc#basic{minnetver = Value};
		"location"  -> Acc#basic{location = Value};
		_               -> {error, {unknown_key, Key, [basic]}}
	    end
    end;

accumulator(info) ->
    fun (_KV, {error, Reason}) -> {error, Reason};
	({Key, Value}, Acc) ->
	    case Key of
		"hostname"     -> Acc#info{hostname = Value};
		"hostport"     -> Acc#info{hostport = Value};
		"maptitle"     -> Acc#info{maptitle = Value};
		"mapname"      -> Acc#info{mapname = Value};
		"gametype"     -> Acc#info{gametype = Value};
		"numplayers"   -> Acc#info{numplayers = Value};
		"maxplayers"   -> Acc#info{maxplayers = Value};
		"gamemode"     -> Acc#info{gamemode = Value};
		"gamever"      -> Acc#info{gamever = Value};
		"minnetver"    -> Acc#info{minnetver = Value};
		"worldlog"     -> Acc#info{worldlog = Value};
		"wantworldlog" -> Acc#info{wantworldlog = Value};
		_                  -> {error, {unknown_key, Key, [info]}}
	    end
    end;

accumulator(rules) ->
    fun (_KV, {error, Reason}) -> {error, Reason};
	({Key, Value}, Acc) ->
	    case Key of
		"mutators"            -> Acc#rules{mutators = Value};
		"listenserver"        -> Acc#rules{listenserver = Value};
		"password"            -> Acc#rules{password = Value};
		"tournament"          -> Acc#rules{tournament = Value};
		"gamestyle"           -> Acc#rules{gamestyle = Value};
		"AdminName"           -> Acc#rules{'AdminName' = Value};
		"AdminEMail"          -> Acc#rules{'AdminEMail' = Value};
		"timelimit"           -> Acc#rules{timelimit = Value};
		"fraglimit"           -> Acc#rules{fraglimit = Value};
		"goalteamscore"       -> Acc#rules{goalteamscore = Value};
		"minplayers"          -> Acc#rules{minplayers = Value};
		"changelevels"        -> Acc#rules{changelevels = Value};
		"maxteams"            -> Acc#rules{maxteams = Value};
		"balanceteams"        -> Acc#rules{balanceteams = Value};
		"playersbalanceteams" -> Acc#rules{playersbalanceteams = Value};
		"friendlyfire"        -> Acc#rules{friendlyfire = Value};
		"gamestype"           -> Acc#rules{gamestype = Value};
		"botskill"            -> Acc#rules{botskill = Value};
		_                         -> {error, {unknown_key, Key, [rules]}}
	    end
    end;

accumulator(players) ->
    fun (_KV, {error, Reason}) -> {error, Reason};
	({Key, Value}, Acc) ->
	    case Key of
		"player_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{player = Value}});
		"frags_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{frags = Value}});
		"ping_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{ping = Value}});
		"team_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{team = Value}});
		"mesh_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{mesh = Value}});
		"skin_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{skin = Value}});
		"face_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{face = Value}});
		"ngsecret_" ++ ID ->
		    Player = get_player(Acc, ID),
		    lists:keystore(ID, 1, Acc, {ID, Player#player{ngsecret = Value}});
		_ -> {error, {unknown_key, Key, [players]}}
	    end
    end;

accumulator(status) ->
    BAccFun = accumulator(basic),
    IAccFun = accumulator(info),
    RAccFun = accumulator(rules),
    PAccFun = accumulator(players),
    fun (KV, Acc = #status{rules = undefined}) ->
	    case PAccFun(KV, Acc#status.players) of
		{error, {unknown_key, _, _}} ->
		    Acc#status{
		      rules = #rules{} = RAccFun(KV, initial_value(rules))};
		Players ->
		    Acc#status{players = Players}
	    end;
	(KV, Acc = #status{info = undefined}) ->
	    case RAccFun(KV, Acc#status.rules) of
		{error, {unknown_key, _, _}} ->
		    Acc#status{
		      info = #info{} = IAccFun(KV, initial_value(info))};
		Rules ->
		    Acc#status{rules = Rules}
	    end;
	(KV, Acc = #status{basic = undefined}) ->
	    case IAccFun(KV, Acc#status.info) of
		{error, {unknown_key, _, _}} ->
		    Acc#status{
		      basic = #basic{} = BAccFun(KV, initial_value(basic))};
		Info ->
		    Acc#status{info = Info}
	    end;
	(KV, Acc) ->
	    Acc#status{basic = #basic{} = BAccFun(KV, Acc#status.basic)}
    end.


initial_value(basic)   -> #basic{};
initial_value(info)    -> #info{};
initial_value(rules)   -> #rules{};
initial_value(players) -> [];
initial_value(status)  -> #status{basic = undefined,
				  info = undefined,
				  rules = undefined,
				  players = initial_value(players)}.


get_player(Acc, P) ->
    case lists:keysearch(P, 1, Acc) of
	{value, {P, Player}} -> Player;
	false -> #player{}
    end.
