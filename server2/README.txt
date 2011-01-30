reference: http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features

for f in iserve/src/*.erl ; do erlc -I iserve/include +debug_info -o iserve/ebin $f; done
ERL_LIBS="$(pwd)" erl -boot start_sasl -mnesia dir mnesia -s mnesia start

testing

    mnesia:create_schema([node()]).
    iserve:create_table().
    iserve:add_callback(8081, 'GET', "/", test_iserve_app, do_get).
    iserve:print_callbacks().
    application:start(iserve).

    % curl localhost:8081

    halt().

some verifications

    iserve:print_callbacks().
    application:load(iserve).
    code:lib_dir(iserve).
