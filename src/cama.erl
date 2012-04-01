%% Author: Sungjin Park <jinni.park@gmail.com>
%% Created: Feb 26, 2012
%% Description: 
-module(cama).
-author("Sungjin Park <jinni.park@gmail.com>").

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([prepare/0, start/0, stop/0]).

%%
%% API Functions
%%
prepare() ->
	ok.

start() ->
	mnesia:start(),
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).

%%
%% Local Functions
%%

