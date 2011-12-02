
%-define(debug_log_macros, false).
%-undef(debug_log_macros).

-define(log_fmt, "~p:~p ~p: ~s").

-ifdef(debug_log_macros).
-define(log0(Type, Msg), (fun()->true=is_list(Msg),error_logger:Type(?log_fmt,[?MODULE,?LINE,self(),Msg])end)()).
-define(log1(Type, Fmt, Args), (fun()->F_log=Fmt,A_log=Args,true=is_list(F_log),true=is_list(A_log),error_logger:Type(?log_fmt,[?MODULE,?LINE,self(),io_lib:format(F_log, A_log)])end)()).
-else. % debug_log_macros
-define(log0(Type, Msg), true).
-define(log1(Type, Fmt, Args), true).
-endif. % debug_log_macros

-define(log_err0(M_log_err), ?log0(error_msg, M_log_err)).
-define(log_err1(F_log_err, A_log_err), ?log1(error_msg, F_log_err, A_log_err)).
-define(log_msg0(M_log_err), ?log0(info_msg, M_log_err)).
-define(log_msg1(F_log_err, A_log_err), ?log1(info_msg, F_log_err, A_log_err)).
-define(log_wrn0(M_log_err), ?log0(warning_msg, M_log_err)).
-define(log_wrn1(F_log_err, A_log_err), ?log1(warning_msg, F_log_err, A_log_err)).
