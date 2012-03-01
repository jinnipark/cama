%% Author: jinni
%% Created: Feb 26, 2012
%% Description: TODO: Add description to cama_home
-module(cama_home).
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
-export([out/1]).

%%
%% API Functions
%%
out(Arg) ->
	case Arg#arg.req#http_request.method of
		'GET' ->
			case ?TRACE(yaws_api:find_cookie_val("guid", Arg#arg.headers#headers.cookie)) of
				[] -> % No login cookie, redirect the user to login page.
					[{status, 302},
					 cama_dispatcher:server_header(Arg),
					 cama_dispatcher:location(Arg, "/login")];
				Guid -> % Login cookie is given, validate it.
					case ?TRACE(cama_login:retrieve_guid(Guid)) of
						{ok, Found} -> % Cookie found
							% Now check if it's stale or not.
							T = ?TRACE(timer:now_diff(now(), Found#cama_guid.last_used)),
							case T < Arg#arg.state#cama_context.login_timeout * 1000000 of
								true -> % Still fresh
									?TRACE(cama_login:update_guid(Guid)),
									[{status, 200},
									 cama_dispatcher:server_header(Arg),
									 yaws_api:setcookie("guid", Guid, cama_dispatcher:base_path(Arg),
														cama_login:quote(cama_login:age2expire(Arg#arg.state#cama_context.login_timeout))),
									 cama_dispatcher:ssi(Arg, "home.html")];
								_ -> % Stale
									cama_login:delete_guid(Guid),
									[{status, 302},
									 cama_dispatcher:server_header(Arg),
									 yaws_api:setcookie("guid", Guid, cama_dispatcher:base_path(Arg),
														cama_login:quote(cama_login:age2expire(0))), % Expire the cookie right away
									 cama_dispatcher:location(Arg, "/login")]
							end;
						_ -> % No such cookie.
							[{status, 302},
							 cama_dispatcher:server_header(Arg),
							 yaws_api:setcookie("guid", Guid, cama_dispatcher:base_path(Arg),
												cama_login:quote(cama_login:age2expire(0))),
							 cama_dispatcher:location(Arg, "/login")]
					end
			end;
		_ ->
			[{status, 405},
			 cama_dispatcher:server_header(Arg)]
	end.

%%
%% Local Functions
%%

