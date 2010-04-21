
%-define(debug_log_macros, false).
%-undef(debug_log_macros).

-define(log_fmt, "~p:~p ~p: ~s").

-ifdef(debug_log_macros).
-define(log0(Type, Msg), true=is_list(Msg), error_logger:Type(?log_fmt, [?MODULE, ?LINE, self(), Msg])).
-define(log1(Type, Fmt, Args), true=is_list(Fmt), true=is_list(Args), error_logger:Type(?log_fmt, [?MODULE, ?LINE, self(), io_lib:format(Fmt, Args)])).
-else. % debug_log_macros
-define(log0(Type, Msg), true).
-define(log1(Type, Fmt, Args), true).
-endif. % debug_log_macros

-define(log_err0(M), ?log0(error_msg, M)).
-define(log_err1(F, A), ?log1(error_msg, F, A)).
-define(log_msg0(M), ?log0(info_msg, M)).
-define(log_msg1(F, A), ?log1(info_msg, F, A)).
-define(log_wrn0(M), ?log0(warning_msg, M)).
-define(log_wrn1(F, A), ?log1(warning_msg, F, A)).
