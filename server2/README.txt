reference: http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features

this should not happen (iserve:create_table/1 is failling)

    > rd(iserve_callback, {key, mf}).
    > mnesia:create_table(iserve_callback, [{attributes, record_info(fields, iserve_callback)}]).

for f in iserve/src/*.erl ; do erlc -I iserve/include +debug_info -o iserve/ebin $f; done
ERL_LIBS="$(cygpath -am .)" erl -boot start_sasl
> mnesia:start().
> appmon:start().                       (optional)
> application:load(iserve).             (optional)
> code:lib_dir(iserve).                 (check path)
> iserve:create_table([node()]).
> iserve:print_callbacks().
> iserve:add_callback(8080, 'GET', "/", test_iserve_app, do_get).
> application:start(iserve).
