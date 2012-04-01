%% Author: jinni
%% Created: Feb 26, 2012
%% Description: cama login/logout module
-module(cama_login).
-author("Sungjin Park <jinni.park@gmail.com>").

%%
%% Include files
%%
-include("cama_api.hrl").
-include("cama_account.hrl").
-include("yaws_api.hrl").
-include("log.hrl").

%%
%% Exported Functions
%%
-export([out/1]).
-export([create_account/3, retrieve_account/1, update_account/2, update_account/1, delete_account/1]).
-export([create_guid/1, retrieve_guid/1, update_guid/1, delete_guid/1]).
-export([age2expire/1, quote/1]).

%%
%% API Functions
%%
out(Arg) ->
	case Arg#arg.req#http_request.method of
		'GET' -> % Present a login page.
			case ?TRACE(yaws_api:find_cookie_val("guid", Arg#arg.headers#headers.cookie)) of
				[] ->
					[{status, 200},
					 cama_dispatcher:server_header(Arg),
					 cama_dispatcher:ssi(Arg, "login.yaws"),
					 {ehtml, [{p, [], "Please enter your credentials..."}]}];
				Guid -> % Logout first if already logged in.
					case retrieve_guid(Guid) of
						{ok, _Found} -> % Already logged in.
							delete_guid(Guid);
						_ -> % Client has a stale cookie.
							ok
					end,
					[{status, 200},
					 cama_dispatcher:server_header(Arg),
					 % Let the client expire the cookie immediately.
					 yaws_api:setcookie("guid", Guid, cama_dispatcher:base_path(Arg), quote(age2expire(0))),
					 cama_dispatcher:ssi(Arg, "login.yaws"),
					 {ehtml, [{p, [], "Logged out..."}]}]
			end;
		'POST' -> % Process login form.
			Form = ?TRACE(yaws_api:parse_post(Arg)),
			Id = proplists:get_value("id", Form),
			case ?TRACE(retrieve_account(Id)) of
				{ok, #cama_account{password=Password}} -> % id is found.
					case proplists:get_value("password", Form) of
						Password -> % password matched.
							% Create a login cookie now.
							case ?TRACE(create_guid(Id)) of
								{ok, Guid} ->
									?TRACE(update_account(Id)),
									[{status, 302},
									 cama_dispatcher:server_header(Arg),
									 cama_dispatcher:location(Arg, "/"),
									 yaws_api:setcookie("guid", Guid#cama_guid.guid, cama_dispatcher:base_path(Arg),
														quote(age2expire(Arg#arg.state#cama.login_timeout)))];
								_ ->
									[{status, 503},
									 cama_dispatcher:server_header(Arg),
									 cama_dispatcher:ssi(Arg, "login.yaws"),
									 {ehtml, [{p, [], "Server failed creating a cookie..."}]}]
							end;
						_ -> % Password doesn't match.
							[{status, 403},
							 cama_dispatcher:server_header(Arg),
							 cama_dispatcher:ssi(Arg, "login.yaws"),
							 {ehtml, [{p, [], "Wrong password..."}]}]
					end;
				_ -> % No such account
					[{status, 403},
					 cama_dispatcher:server_header(Arg),
					 cama_dispatcher:ssi(Arg, "login.yaws"),
					 {ehtml, [{p, [], "Invalid id..."}]}]
			end;
		_ ->
			[{status, 405},
			 cama_dispatcher:server_header(Arg)]
	end.

%% @doc Create an account
%% @spec create_account(Id=string(), Password=string(), Email=string()) -> {ok, #cama_account()} | error
create_account(Id, Password, Email) ->
	Account = #cama_account{id=Id, password=Password, email=Email, created=now()},
	case mnesia:transaction(fun() -> mnesia:write(Account) end) of
		{atomic, ok} ->
			{ok, Account};
		_ ->
			error
	end.

%% @doc Retrieve an account
%% @spec retrieve_account(Id=string()) -> {ok, #cama_account()} | error
retrieve_account(Id) ->
	case mnesia:transaction(fun() -> mnesia:read({cama_account, Id}) end) of
		{atomic, [Account]} ->
			{ok, Account};
		_ ->
			error
	end.

%% @doc Change password of an account
%% @spec update_account(Id=string(), Password=string()) -> {ok, #cama_account()} | error
update_account(Id, Password) ->
	F = fun() ->
				case mnesia:read({cama_account, Id}) of
					{ok, [Account]} ->
						Account1 = Account#cama_account{password=Password},
						case mnesia:write(Account1) of
							ok ->
								Account1;
							_ ->
								error
						end;
					_ ->
						error
				end
		end,
	case mnesia:transaction(F) of
		{atomic, error} ->
			error;
		{atomic, Account} ->
			{ok, Account};
		_ ->
			error
	end.

%% @doc Update login history of an account.
%% @spec update_account(Id=string()) -> {ok, #cama_account()} | error
update_account(Id) ->
	F = fun() ->
				case mnesia:read({cama_account, Id}) of
					[Account=#cama_account{logins=Logins}] ->
						Account1 = Account#cama_account{logins=Logins+1, last_login=now()},
						case mnesia:write(Account1) of
							ok ->
								Account1;
							_ ->
								error
						end;
					_ ->
						error
				end
		end,
	case mnesia:transaction(F) of
		{atomic, error} ->
			error;
		{atomic, Account} ->
			{ok, Account};
		_ ->
			error
	end.

%% @doc Delete an account.
%% @spec delete_account(Id=string()) -> ok | error
delete_account(Id) ->
	case mnesia:transaction(fun() -> mnesia:delete({cama_account, Id}) end) of
		{atomic, ok} ->
			ok;
		_ ->
			error
	end.

%% @doc Create a login cookie.
%% @spec create_guid(Id=string()) -> {ok, #cama_guid()} | error
create_guid(Id) ->
	random:seed(now()),
	Guid = #cama_guid{guid=uuid:to_string(uuid:v4()),
					  id=Id,
					  created=now(),
					  last_used=now()},
	case mnesia:dirty_write(Guid) of
		ok ->
			{ok, Guid};
		_ ->
			error
	end.

%% @doc Retrieve a login cookie.
%% @spec retrieve_guid(Guid=string()) -> {ok, #cama_guid()} | error
retrieve_guid(Guid) ->
	case mnesia:dirty_read({cama_guid, Guid}) of
		[] ->
			error;
		[Found] ->
			{ok, Found}
	end.

%% @doc Update a login cookie's last used time.
%% @spec update_guid(Guid=string()) -> {ok, #cama_guid()} | error
update_guid(Guid) ->
	case mnesia:dirty_read({cama_guid, Guid}) of
		[] ->
			error;
		[Found] ->
			Found1 = Found#cama_guid{last_used=now()},
			{ok, Found1}
	end.

%% @doc Delete a login cookie.
%% @spec delete_guid(Guid=string()) -> ok | error
delete_guid(Guid) ->
	mnesia:dirty_delete({cama_guid, Guid}).

%% @doc Converts Age in seconds to HTTP-datetime format.
%% @spec age2expire(integer()) -> string()
age2expire(Age) ->
	Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	Exp = Now + Age,
	httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(Exp)).

quote(Str) ->
	"\"" ++ Str ++ "\"".

%%
%% Local Functions
%%

