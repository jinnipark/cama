%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Account schema
%%%
%%% Created : Feb 26, 2012
%%% -------------------------------------------------------------------
-author("Sungjin Park <jinni.park@gmail.com>").

-record(cama_account, {id=undefined,
					   password=undefined,
					   email=undefined,
					   created=undefined,
					   logins=0,
					   last_login=undefined}).
