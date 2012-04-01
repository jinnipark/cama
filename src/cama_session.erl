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

-record(?MODULE, {id,
				  token,
				  created,
				  timeout = 3600,
				  pid}).

-record(state, {}).

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
	{ok, Env} = application:get_env(cama, ?MODULE),
	Session = ?PROPS_TO_RECORD(Env, ?MODULE),
	case Arg#arg.req#http_request.method of
		'PUT' -> % allocate
			allocate(Arg, Session);
		_ ->
			[{status, 405},
			 cama_dispatcher:server_header(Arg)]
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
init(_Session) ->
    {ok, state_name, #state{}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
state_name(Event, StateData) ->
    {next_state, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
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
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
allocate(Arg, Session) ->
	random:seed(now()),
	Session1 = Session#?MODULE{id = uuid:to_string(uuid:v4()),
							   token = uuid:to_string(uuid:v4()),
							   created = now()},
	{ok, Pid} = gen_fsm:start(?MODULE, Session1, []),
	Session2 = Session1#?MODULE{pid = Pid},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Session2) end),
	Location = cama_dispatcher:base_path(Arg) ++ "/session/" ++ Session2#?MODULE.id,
	[{status, 201},
	 cama_dispatcher:server_header(Arg),
	 {header, "X-Type: basic"},
	 {header, {location, Location}},
	 yaws_api:setcookie("token", Session2#?MODULE.token,
						Location, age2expire(Session2#?MODULE.timeout))].

age2expire(Age) ->
	Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	Exp = Now + Age,
	httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(Exp)).

quote(Str) ->
	[$", Str, $"].
