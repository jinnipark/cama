%% Author: jinni
%% Created: Apr 1, 2012
%% Description: TODO: Add description to cama_tests
-module(cama_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

-define(BASE_URL, "http://localhost:8000/cama").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
initialize_test() ->
	inets:start(),
	inets:start(httpc, [{profile, host}]),
	?assertEqual(httpc:set_options([{cookies, enabled}], host), ok),
	inets:start(httpc, [{profile, guest}]),
	?assertEqual(httpc:set_options([{cookies, enabled}], guest), ok).

allocate_test() ->
	URL = ?BASE_URL ++ "/session",
	{ok, {{_, 201, _}, Headers, _}} =
		httpc:request(put, {URL, [], [], []}, [], [], host),
	Location = proplists:get_value("location", Headers),
	?assertNot(Location =:= undefined),
	SessionURL = ?BASE_URL ++ Location,
	%% @todo httpc fails to store cookies appropriately
	{"cookie", Cookie} = httpc:cookie_header(SessionURL, host),
	?assertNot(Cookie =:= []),
	Type = proplists:get_value("x-type", Headers),
	?assertNot(Type =:= undefined),
	{ok, {Location, Type}}.

finalize_test() ->
	?assertEqual(ok, inets:stop(httpc, guest)),
	?assertEqual(ok, inets:stop(httpc, host)).

%%
%% Local Functions
%%

