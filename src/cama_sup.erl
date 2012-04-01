%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%% Description :
%%%
%%% Created : Apr 1, 2012
%%% -------------------------------------------------------------------
-module(cama_sup).
-author("Sungjin Park <jinni.park@gmail.com>").

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
	Env = application:get_all_env(cama),
	Modules = proplists:get_keys(Env),
	lists:foreach(fun(M) -> catch M:prepare() end, Modules),
    {ok, { {one_for_one, 5, 10}, []} }.
