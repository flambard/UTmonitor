-module(utmonitor).

-include("reply.hrl").

-export([start/1]).
-export([init/1]).

-define(SEND_INTERVAL, 5000).
-define(DM_LEFT_PLAYER_COL, {64, 192}).
-define(DM_LEFT_FRAGS_COL, {396, 192}).
-define(CTF_LEFT_PLAYER_COL, {64, 320}).
-define(CTF_LEFT_FRAGS_COL, {396, 320}).
-define(RIGHT_PLAYER_COL, {532, 320}).
-define(RIGHT_FRAGS_COL, {864, 320}).
-define(GAMETYPE_CTF, <<"CTFGame">>).
-define(RED_TEAM, <<"0">>).

-record(texts, {
	  header,
	  map,
	  rules,
	  left_player_col,
	  left_frags_col,
	  right_player_col,
	  right_frags_col,
	  left_team_score,
	  right_team_score
	 }).

start(Server) ->
    spawn(?MODULE, init, [Server]).

init(ServerAddress) ->
    GS = gs:start(),
    Dimensions = [{width, 1000}, {height, 700}],
    W = gs:create(window, GS, [{title, "UTmonitor"}, {configure, true} | Dimensions]),
    F = gs:create(frame, W, [{packer_x, [{stretch, 1}]}, {packer_y, [{stretch, 1}]}]),
    C = gs:create(canvas, F, [{bg, black}, {pack_xy, {1, 1}}]),
    T = #texts{
      header = gs:create(text, C, [{coords, [{8, 8}]}, {font, {verdana, 14}}, {fg, red}, {text, ""}]),
      map = gs:create(text, C, [{coords, [{64, 64}]}, {font, {verdana, bold, 28}}, {fg, red}, {text, ""}]),
      rules = gs:create(text, C, [{coords, [{532, 64}]}, {font, {verdana, 20}}, {fg, red}, {text, ""}]),
      left_player_col = gs:create(text, C, [{coords, [?DM_LEFT_PLAYER_COL]}, {font, {verdana, bold, 24}}, {fg, red}, {text, ""}]),
      left_frags_col = gs:create(text, C, [{coords, [?DM_LEFT_FRAGS_COL]}, {font, {verdana, bold, 24}}, {fg, red}, {text, ""}]),
      right_player_col = gs:create(text, C, [{coords, [?RIGHT_PLAYER_COL]}, {font, {verdana, bold, 24}}, {fg, blue}, {text, ""}]),
      right_frags_col = gs:create(text, C, [{coords, [?RIGHT_FRAGS_COL]}, {font, {verdana, bold, 24}}, {fg, blue}, {text, ""}]),
      left_team_score = gs:create(text, C, [{coords, [{360, 160}]}, {font, {verdana, bold, 100}}, {fg, red}, {text, ""}]),
      right_team_score = gs:create(text, C, [{coords, [{532, 160}]}, {font, {verdana, bold, 100}}, {fg, blue}, {text, ""}])
     },
    gs:config(F, Dimensions),
    gs:config(W, {map, true}),
    process_flag(trap_exit, true),
    Server = utserver:open(ServerAddress),
    erlang:send_after(?SEND_INTERVAL, self(), {timer, send_status_message}),
    loop(Server, F, T).

loop(Server, F, T) ->
    receive
	{'EXIT', Reason} ->
	    io:format("utserver exited with reason: ~w\n", [Reason]),
	    NewServer = utserver:open(utserver:address(Server)),
	    loop(NewServer, F, T);
    	{timer, send_status_message} ->
	    utserver:send(status, Server),
	    erlang:send_after(?SEND_INTERVAL, self(), {timer, send_status_message}),
	    loop(Server, F, T);

	{utserver, #status{info = #info{hostname = Host,
					gametype = GameType,
					mapname = MapName},
			   rules = #rules{gamestyle = Style,
					  fraglimit = Time,
					  timelimit = Frags,
					  goalteamscore = Goal},
			   players = Ps}} ->
	    Sorted = sort_player_list(Ps),
	    case GameType of
		?GAMETYPE_CTF ->
		    %% Separate the players in two teams:
		    {RedTeam, BlueTeam} =
			lists:partition(fun ({_, #player{team = Team}}) ->
						Team =:= ?RED_TEAM
					end,
					Sorted),
		    {LeftPList, LeftFList} = format_player_columns(RedTeam),
		    {RightPList, RightFList} = format_player_columns(BlueTeam),
		    %% Red team left column, blue team right column
		    %% Move down the left player and frags columns
		    gs:config(T#texts.left_player_col, {coords, [?CTF_LEFT_PLAYER_COL]}),
		    gs:config(T#texts.left_frags_col, {coords, [?CTF_LEFT_FRAGS_COL]}),
		    %%gs:config(T#texts.left_team_score, {text, ?X}),
		    %%gs:config(T#texts.right_team_score, {text, ?Y}),
		    gs:config(T#texts.left_player_col, {text, LeftPList}),
		    gs:config(T#texts.left_frags_col, {text, LeftFList}),
		    gs:config(T#texts.right_player_col, {text, RightPList}),
		    gs:config(T#texts.right_frags_col, {text, RightFList}),
		    gs:config(T#texts.rules, {text, io_lib:format("Goal: Capture flag ~s times.", [Goal])});
		_ ->
		    %% Move up the left player and frags columns
		    %% Clear the team scores
		    %% Clear the right player and frags columns
		    %% All players in left column
		    {PList, FList} = format_player_columns(Sorted),
		    gs:config(T#texts.left_player_col, {coords, [?DM_LEFT_PLAYER_COL]}),
		    gs:config(T#texts.left_frags_col, {coords, [?DM_LEFT_FRAGS_COL]}),
		    gs:config(T#texts.left_team_score, {text, ""}),
		    gs:config(T#texts.right_team_score, {text, ""}),
		    gs:config(T#texts.left_player_col, {text, PList}),
		    gs:config(T#texts.left_frags_col, {text, FList}),
		    gs:config(T#texts.right_player_col, {text, ""}),
		    gs:config(T#texts.right_frags_col, {text, ""}),
		    gs:config(T#texts.rules, {text, io_lib:format("~s frags.\n~s minutes.", [Time, Frags])})
	    end,
	    gs:config(T#texts.header, {text, io_lib:format("~s ~s ~s", [Host, Style, GameType])}),
	    gs:config(T#texts.map, {text, binary_to_list(MapName)}),
	    loop(Server, F, T);

	{gs, _W, destroy, _Data, _Args} ->
	    utserver:close(Server),
	    bye;

	{gs, _W, configure, _Data, [W, H | _Args]} ->
	    gs:config(F, [{width, W}, {height, H}]),
	    loop(Server, F, T);

	Slask ->
	    io:format("Got unexpected message: ~p~n", [Slask]),
	    loop(Server, F, T)
    end.

sort_player_list(Ps) ->
    lists:sort(fun ({_, #player{frags = X}}, {_, #player{frags = Y}}) ->
		       Xbits = bit_size(X),
		       Ybits = bit_size(Y),
		       <<A:Xbits>> = X,
		       <<B:Ybits>> = Y,
		       A > B
	       end,
	       Ps).

format_player_columns(Players) ->
    {[io_lib:format("~s~n", [P#player.player]) || {_N, P} <- Players],
     [io_lib:format("~s~n", [P#player.frags])  || {_N, P} <- Players]}.
