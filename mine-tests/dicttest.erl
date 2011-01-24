-module(dicttest).
-export([main/0, main/1]).

% erl -s dicttest main
% erlc dicttest.erl && erl -noshell -s dicttest main -s init stop

main() ->
    main([]).

-include("dicttest.escript").
