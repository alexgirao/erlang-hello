#!/usr/bin/env escript
%% -*- erlang -*-

%$ echo -n "a" | sha1sum
%86f7e437faa5a7fce15d1ddcb9eaeaea377667b8  -

main(_) ->
    crypto:start(),
    <<16#86f7e437faa5a7fce15d1ddcb9eaeaea377667b8:160>> = crypto:sha("a"),
    20 = size(crypto:sha("a")).
