%% Author: jinni
%% Created: Feb 26, 2012
%% Description: Dispatches http requests according to url patterns.
-module(cama_dispatcher).
-author("Sungjin Park <jinni.park@gmail.com>").

%%
%% Include files
%%
-include("cama_api.hrl").
-include("yaws_api.hrl").
-include("log.hrl").
-include("props_to_record.hrl").

%%
%% Exported Functions
%%
-export([out/1]).
-export([server_header/1, ssi/2, location/2, base_path/1]).

%%
%% API Functions
%%
out(Arg) ->
	?DEBUG([{Arg#arg.req#http_request.method, Arg#arg.pathinfo},
			{Arg#arg.client_ip_port, Arg#arg.headers#headers.user_agent},
			{Arg#arg.state, Arg#arg.opaque}]),
	{ok, Env} = application:get_env(cama, context),
	Context = ?PROPS_TO_RECORD(Env, cama_context),
	Arg1 = Arg#arg{state=Context},
	case catch string:tokens(Arg#arg.pathinfo, "/") of
		{'EXIT', _} -> % pathinfo might be undefined
			?TRACE(cama_home:out(Arg1));
		[] ->
			?TRACE(cama_home:out(Arg1));
		["login"] ->
			?TRACE(cama_login:out(Arg1));
		["sessions"] ->
			?TRACE(cama_sessions:out(Arg1));
		["session", Sid] ->
			?TRACE(cama_session:out(Arg1#arg{state=Context#cama_context{sid=Sid}}));
		_ ->
			?DEBUG([server_header(Arg1),
					ssi(Arg1, Arg1#arg.pathinfo)])
	end.

server_header(Arg) ->
	{header, "server:" ++ Arg#arg.state#cama_context.server}.

ssi(Arg, Path) ->
	yaws_api:ssi(filename:join(code:priv_dir(cama), Arg#arg.state#cama_context.docroot), [Path]).

location(Arg, Path) ->
	{header, {location, base_path(Arg) ++ Path}}.

base_path(Arg) ->
	case catch string:rstr(Arg#arg.server_path, Arg#arg.pathinfo) of
		{'EXIT', _} ->
			Arg#arg.server_path;
		N ->
			string:substr(Arg#arg.server_path, 1, N - 1)
	end.

%%
%% Local Functions
%%

