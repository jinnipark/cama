%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Some log macros.
%%%   ?DEBUG(Exp) -> evaluates Exp, prints a debug message after the evaluation
%%%					if 'DEBUG' is defined at compile time.
%%%   ?TRACE(Exp) -> evaluates Exp, prints a debug message before and after the
%%%					evaluation if 'DEBUG' is defined at compile time.
%%%   ?INFO(Message) -> print an info message
%%%   ?WARNING(Message) -> print a warning message
%%%   ?ERROR(Message) -> print an error message
%%%
%%% Created : Feb 26, 2012
%%% -------------------------------------------------------------------
-author("Sungjin Park <jinni.park@gmail.com>").

-ifdef(DEBUG).

%% @doc Evaluates Exp, prints a debug message after the evaluation if 'DEBUG' is
%% defined at compile time.
-define(DEBUG(Exp), ?DEBUG_FUN(Exp)()).

%% @doc Evaluates Exp, prints a debug message before and after the evaluation if
%% 'DEBUG' is defined at compile time.
-define(TRACE(Exp), ?TRACE_FUN(Exp)(error_logger:info_msg("~s: ~p~n[TRACE ~p] ~p", [?MODULE, ?LINE, self(), ??Exp]))).
-define(DEBUG_FUN(Exp),
		fun() ->
				Var = Exp, % It's essential to bind to a Var here to avoid evaluating Exp twice. 
				?DEBUG_OUT(Var),
				Var
		end).
-define(TRACE_FUN(Exp),
		fun(_Init) -> % Init is here just as a placeholder, guarantees to be evaluated before this is called.
				Var = Exp, % It's essential to bind to a Var here to avoid evaluating Exp twice.
				?DEBUG_OUT(Var),
				Var
		end).
-define(DEBUG_OUT(Msg), error_logger:info_msg("~s: ~p~n[DEBUG ~p] ~p", [?MODULE, ?LINE, self(), Msg])).

-else.

-define(DEBUG(Exp), Exp).
-define(TRACE(Exp), Exp).

-endif.

-define(INFO(Report), error_logger:info_report([{?MODULE, ?LINE}, Report])).
-define(WARNING(Report), error_logger:warning_report([{?MODULE, ?LINE}, Report])).
-define(ERROR(Report), error_logger:error_report([{?MODULE, ?LINE}, Report])).
