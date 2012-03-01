%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Common definitions in cama
%%%
%%% Created : Feb 26, 2012
%%% -------------------------------------------------------------------
-author("Sungjin Park <jinni.park@gmail.com>").

-record(cama_context, {server="cama/1",
					   docroot="www",
					   login_timeout=3600, % 1h by default
					   sid = undefined % session id
					  }).

-record(cama_guid, {guid=undefined,
					id=undefined, % owner
					created=undefined,
					last_used=undefined
				   }).
