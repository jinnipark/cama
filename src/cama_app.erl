%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%% Description :
%%%
%%% Created : Apr 1, 2012
%%% -------------------------------------------------------------------
-module(cama_app).
-author("Sungjin Park <jinni.park@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cama_sup:start_link().

stop(_State) ->
    ok.
