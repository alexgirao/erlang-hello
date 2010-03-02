reference: http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features

for f in iserve/src/*.erl ; do erlc -I iserve/include +debug_info -o iserve/ebin $f; done
ERL_LIBS="$(cygpath -am .)" erl -boot start_sasl
> application:load(iserve).
> code:lib_dir(iserve).
> application:start(iserve).
