%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%% Description :
%%%
%%% Created : Apr 1, 2012
%%% -------------------------------------------------------------------
-module(cama_session).
-author("Sungjin Park <jinni.park@gmail.com>").

-behaviour(gen_fsm).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("cama_api.hrl").
-include("yaws_api.hrl").
-include("log.hrl").
-include("props_to_record.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([prepare/0, out/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export(['IDLE'/2, 'IDLE'/3, 'BUSY'/2, 'BUSY'/3]).

-record(?MODULE, {id,
				  token,
				  created,
				  timeout = 3600,
				  request_timeout = 300,
				  type = basic,
				  pid,
				  timer,
				  hostr,
				  hosts,
				  guestr,
				  guests}).

%% ====================================================================
%% External functions
%% ====================================================================
prepare() ->
	?TRACE(mnesia:create_table(?MODULE,
							   [{attributes, record_info(fields, ?MODULE)},
								{ram_copies, [node()]}])),
	?TRACE(mnesia:wait_for_tables([?MODULE], infinity)).

out(Arg) ->
	?DEBUG([Arg#arg.state]),
	case Arg#arg.state#cama.sid of
		undefined -> % allocation operation
			{ok, Env} = application:get_env(cama, ?MODULE),
			Session = ?PROPS_TO_RECORD(Env, ?MODULE),
			case Arg#arg.req#http_request.method of
				'PUT' -> % allocate
					allocate(Arg, Session);
				_ ->
					[{status, 405},
					 cama_dispatcher:server_header(Arg)]
			end;
		Sid -> % relay operation
			case Arg#arg.req#http_request.method of
				'GET' -> % recv
					recv(Arg, Sid);
				_ ->
					[{status, 405},
					 cama_dispatcher:server_header(Arg)]
			end
	end.

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init(Session) ->
	process_flag(trap_exit, true),
	Session1 = Session#?MODULE{pid = self()},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Session1) end),
	Session2 = Session1#?MODULE{timer = gen_fsm:start_timer(Session1#?MODULE.timeout*1000, expired)},
	?INFO(["session initiated", Session2]),
    {ok, 'IDLE', Session2}.

%% --------------------------------------------------------------------
%% Func: 'IDLE'/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
'IDLE'({timeout, Timer, _}, StateData = #?MODULE{timer = Timer}) ->
	{stop, normal, StateData};
'IDLE'(Event, StateData) ->
	?WARNING(["undefined async event", Event]),
    {next_state, 'IDLE', StateData}.

%% --------------------------------------------------------------------
%% Func: 'BUSY'/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
'BUSY'(Event, StateData) ->
	?WARNING(["undefined async event", Event]),
    {next_state, 'BUSY', StateData}.

%% --------------------------------------------------------------------
%% Func: 'IDLE'/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
'IDLE'(host_start_receiving, {Receiver, _}, Session) ->
	?DEBUG(["host", Session#?MODULE.id, "started receiving from", Receiver]),
	link(Receiver),
	gen_fsm:cancel_timer(Session#?MODULE.timer),
	{reply, ok, 'BUSY', Session#?MODULE{hostr=Receiver, timer=undefined}};
'IDLE'(guest_start_receiving, {Receiver, _}, Session) ->
	?DEBUG(["guest", Session#?MODULE.id, "started receiving from", Receiver]),
	link(Receiver),
	gen_fsm:cancel_timer(Session#?MODULE.timer),
	{reply, ok, 'BUSY', Session#?MODULE{guestr=Receiver, timer=undefined}};
'IDLE'(Event, From, StateData) ->
	?WARNING(["undefined sync event", Event, From]),
    Reply = ok,
    {reply, Reply, 'IDLE', StateData}.

%% --------------------------------------------------------------------
%% Func: 'BUSY'/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
'BUSY'(host_start_receiving, {Receiver, _}, Session=#?MODULE{hostr=Hostr}) ->
	?DEBUG(["host", Session#?MODULE.id, "started receiving from", Receiver]),
	case (Hostr =:= undefined) or (Hostr =:= Receiver) of
		true ->
			link(Receiver),
			case Session#?MODULE.guests of
				undefined ->
					ok;
				Sender ->
					Sender ! {receiver_arrived, Receiver}
			end,
			{reply, ok, 'BUSY', Session#?MODULE{hostr=Receiver}};
		_ ->
			?WARNING(["another host", Hostr, "is already receiving"]),
			{reply, occupied, 'BUSY', Session}
	end;
'BUSY'(guest_start_receiving, {Receiver, _}, Session=#?MODULE{guestr=Guestr}) ->
	?DEBUG(["guest", Session#?MODULE.id, "started receiving from", Receiver]),
	case (Guestr =:= undefined) or (Guestr =:= Receiver) of
		true ->
			link(Receiver),
			case Session#?MODULE.hosts of
				undefined ->
					ok;
				Sender ->
					Sender ! {receiver_arrived, Receiver}
			end,
			{reply, ok, 'BUSY', Session#?MODULE{guestr=Receiver}};
		_ ->
			?WARNING(["another guest", Guestr, "is already receiving"]),
			{reply, occupied, 'BUSY', Session}
	end;
'BUSY'(Event, From, StateData) ->
	?WARNING(["undefined sync event", Event, From]),
    Reply = ok,
    {reply, Reply, 'BUSY', StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	?WARNING(["undefined anonymous async event", Event]),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
	?WARNING(["undefined anonymous sync event", Event]),
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, 'BUSY', Session=#?MODULE{hostr=Hostr, hosts=Hosts, guestr=Guestr, guests=Guests}) ->
	case Pid of
		Hostr ->
			?DEBUG(["host", Session#?MODULE.id, "stopped receiving from", Pid]),
			case (Hosts =:= undefined) and (Guestr =:= undefined) and (Guests =:= undefined) of
				true ->
					{next_state, 'IDLE', Session#?MODULE{hostr=undefined, timer=gen_fsm:start_timer(Session#?MODULE.timeout*1000, expired)}};
				_ ->
					{next_state, 'BUSY', Session#?MODULE{hostr=undefined}}
			end;
		Hosts ->
			?DEBUG(["host", Session#?MODULE.id, "stopped sending from", Pid]),
			case (Hostr =:= undefined) and (Guestr =:= undefined) and (Guests =:= undefined) of
				true ->
					{next_state, 'IDLE', Session#?MODULE{hosts=undefined, timer=gen_fsm:start_timer(Session#?MODULE.timeout*1000, expired)}};
				_ ->
					{next_state, 'BUSY', Session#?MODULE{hosts=undefined}}
			end;
		Guestr ->
			?DEBUG(["guest", Session#?MODULE.id, "stopped receiving from", Pid]),
			case (Hostr =:= undefined) and (Hosts =:= undefined) and (Guests =:= undefined) of
				true ->
					{next_state, 'IDLE', Session#?MODULE{guestr=undefined, timer=gen_fsm:start_timer(Session#?MODULE.timeout*1000, expired)}};
				_ ->
					{next_state, 'BUSY', Session#?MODULE{guestr=undefined}}
			end;
		Guests ->
			?DEBUG(["guest", Session#?MODULE.id, "stopped sending from", Pid]),
			case (Hosts =:= undefined) and (Hostr =:= undefined) and (Guestr =:= undefined) of
				true ->
					{next_state, 'IDLE', Session#?MODULE{guests=undefined, timer=gen_fsm:start_timer(Session#?MODULE.timeout*1000, expired)}};
				_ ->
					{next_state, 'BUSY', Session#?MODULE{guests=undefined}}
			end;
		_ ->
			?WARNING(["session", Session#?MODULE.id, "received unknown exit from", Pid]),
			{next_state, 'BUSY', Session}
	end;
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, Session) ->
	mnesia:transaction(fun() -> mnesia:delete({?MODULE, Session#?MODULE.id}) end),
	?INFO(["session terminated", Session]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
allocate(Arg, Session) ->
	random:seed(now()),
	Session1 = Session#?MODULE{id = uuid:to_string(uuid:v4()),
							   token = uuid:to_string(uuid:v4()),
							   created = now()},
	% Query might contain secure=true (defaults to false).
	Session2 = case yaws_api:queryvar(Arg, "secure") of
				   {ok, "true"} ->
					   Session1#?MODULE{type=secure};
				   _ ->
					   Session1
			   end,
	% Query might also contain timeout=100 (seconds).
	Session3 = case yaws_api:queryvar(Arg, "timeout") of
				   {ok, Int} ->
					   case string:to_integer(Int) of
						   {error, _} ->
							   Session2;
						   {Timeout, _} ->
							   Session2#?MODULE{timeout=Timeout}
					   end;
				   _ ->
					   Session2
			   end,
	{ok, _} = gen_fsm:start(?MODULE, Session3, []),
	Location = cama_dispatcher:base_path(Arg) ++ Arg#arg.pathinfo ++ "/" ++ Session3#?MODULE.id,
	[{status, 201},
	 cama_dispatcher:server_header(Arg),
	 {header, io_lib:format("X-Type: ~w", [Session3#?MODULE.type])},
	 {header, {location, Location}},
	 yaws_api:setcookie("token", Session2#?MODULE.token,
						Location, age2expire(Session3#?MODULE.timeout))].

recv(Arg, Sid) ->
	Location = cama_dispatcher:base_path(Arg) ++ Arg#arg.pathinfo ++ "/" ++ Sid,
	case mnesia:transaction(fun() -> mnesia:read({?MODULE, Sid}) end) of
		{atomic, []} -> % session not found, might have been expired
			[{status, 404},
			 cama_dispatcher:server_header(Arg),
			 yaws_api:setcookie("token", "", Location, age2expire(0))]; % delete token if there is any
		{atomic, [Session]} ->
			case catch yaws_api:find_cookie_val("token", Arg) of
				Token when Token =:= Session#?MODULE.token -> % host
					host_recv(Arg, Session);
				_ -> % guest
					guest_recv(Arg, Session)
			end;
		Error -> % database error
			?ERROR(["mnesia error", Error]),
			[{status, 500},
			 cama_dispatcher:server_header(Arg)]
	end.

%% Host registers itself to session and start receiving.
host_recv(Arg, Session) ->
	case gen_fsm:sync_send_event(Session#?MODULE.pid, host_start_receiving) of
		ok ->
			Timeout = case yaws_api:queryvar(Arg, "timeout") of
						  {ok, Val} ->
							  case string:to_integer(Val) of
								  {error, _} ->
									  Session#?MODULE.request_timeout;
								  {Int, _} ->
									  Int
							  end;
						  _ ->
							  Session#?MODULE.request_timeout
					  end,
			Location = cama_dispatcher:base_path(Arg) ++ Arg#arg.pathinfo ++ "/" ++ Session#?MODULE.id,
			receive
				{chunk, ContentType, Data} ->
					[{status, 200},
					 cama_dispatcher:server_header(Arg),
					 yaws_api:setcookie("token", Session#?MODULE.token,
										Location, age2expire(Session#?MODULE.timeout)),
					 {streamcontent, ContentType, Data}];
				{data, ContentType, Data} ->
					[{status, 200},
					 cama_dispatcher:server_header(Arg),
					 {content, ContentType, Data}]
			after Timeout*1000 ->
					[{status, 504},
					 cama_dispatcher:server_header(Arg),
					 yaws_api:setcookie("token", Session#?MODULE.token,
										Location, age2expire(Session#?MODULE.timeout)),
					 {content, "text/plain", "request timed out"}]
			end;
		occupied ->
			[{status, 409},
			 cama_dispatcher:server_header(Arg),
			 {content, "text/plain", "there can be only one receiver at a time"}];
		_ ->
			[{status, 500},
			 cama_dispatcher:server_header(Arg),
			 {content, "text/plain", "session doesn't resond"}]
	end.

%% Guest registers itself to session and start receiving.
guest_recv(Arg, Session) ->
	case gen_fsm:sync_send_event(Session#?MODULE.pid, guest_start_receiving) of
		ok ->
			Timeout = case yaws_api:queryvar(Arg, "timeout") of
						  {ok, Val} ->
							  case string:to_integer(Val) of
								  {error, _} ->
									  Session#?MODULE.request_timeout;
								  {Int, _} ->
									  Int
							  end;
						  _ ->
							  Session#?MODULE.request_timeout
					  end,
			receive
				{chunk, ContentType, Data} ->
					[{status, 200},
					 cama_dispatcher:server_header(Arg),
					 {streamcontent, ContentType, Data}];
				{data, ContentType, Data} ->
					[{status, 200},
					 cama_dispatcher:server_header(Arg),
					 {content, ContentType, Data}]
			after Timeout*1000 ->
					[{status, 504},
					 cama_dispatcher:server_header(Arg),
					 {content, "text/plain", "request timed out"}]
			end;
		occupied ->
			[{status, 409},
			 cama_dispatcher:server_header(Arg),
			 {content, "text/plain", "there can be only one receiver at a time"}];
		_ ->
			[{status, 500},
			 cama_dispatcher:server_header(Arg),
			 {content, "text/plain", "session doesn't resond"}]
	end.

age2expire(Age) ->
	Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	Exp = Now + Age,
	httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(Exp)).
