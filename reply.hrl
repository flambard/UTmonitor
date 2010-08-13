-record(basic, {
	  gamename,
	  gamever,
	  minnetver,
	  location
	 }).

-record(info, {
	  hostname,
	  hostport,
	  maptitle,
	  mapname,
	  gametype,
	  numplayers,
	  maxplayers,
	  gamemode,
	  gamever,
	  minnetver,
	  worldlog,
	  wantworldlog
	 }).

-record(rules, {
	  mutators,
	  listenserver,
	  password,
	  tournament,
	  gamestyle,
	  'AdminName',
	  'AdminEMail',
	  timelimit,
	  fraglimit,
	  goalteamscore,
	  minplayers,
	  changelevels,
	  maxteams,
	  balanceteams,
	  playersbalanceteams,
	  friendlyfire,
	  gamestype,
	  botskill
	 }).

-record(player, {
	  player,
	  frags,
	  ping,
	  team,
	  mesh,
	  skin,
	  face,
	  ngsecret
	 }).

-record(status, {
	  basic,
	  info,
	  rules,
	  players
	 }).
