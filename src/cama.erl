%% Author: jinni
%% Created: Feb 26, 2012
%% Description: TODO: Add description to cama
-module(cama).
-author("Sungjin Park <jinni.park@gmail.com>").

%%
%% Include files
%%
-include("cama_api.hrl").
-include("yaws_api.hrl").
-include("log.hrl").

%%
%% Exported Functions
%%
-export([start/0, stop/0]).

%%
%% API Functions
%%
start() ->
	?TRACE(mnesia:start()),
	cama_login:start(),
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).

%%
%% Local Functions
%%

