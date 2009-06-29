-module(http_util_elias).
-author('Elias Torres <elias@torrez.us>').

-record(url, {abspath, scheme, netloc, path, search, fragment}).
    
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

urljoin([], Url) ->
    Url;

urljoin(Base, []) ->
    Base;
    
urljoin(Base, Url) ->
  SplitBase = urlsplit(Base),
  SplitUrl = urlsplit(Url),
  Splitted = if
    SplitUrl#url.scheme =/= SplitBase#url.scheme, SplitUrl#url.scheme =/= undefined -> 
        SplitUrl;
    true ->
        if 
            SplitUrl#url.netloc =/= undefined ->
                SplitUrl#url{scheme=SplitBase#url.scheme};
          
            true ->
                %%?debugVal(SplitUrl),
                WithPath = if
                  SplitUrl#url.path =:= undefined ->
                      Query = if
                          SplitUrl#url.search =/= undefined ->
                              SplitUrl#url.search;
                          true ->
                              SplitBase#url.search
                      end,
                      #url{path=SplitBase#url.path, search=Query};
                  true ->
                      case SplitUrl#url.path of
                          [$/ | _Rest ]  ->
                              SplitUrl#url{path=remove_dot_segments(SplitUrl#url.path)};
                          Path ->
                              MergedPath = merge_path(SplitBase, SplitUrl),
                              %%?debugVal(MergedPath),
                              CleanedPath = remove_dot_segments(MergedPath),
                              SplitUrl#url{path=CleanedPath}
                      end
                end,
                WithPath#url{scheme=SplitBase#url.scheme, 
                    netloc=SplitBase#url.netloc}
        end
  end,
  urlunsplit(Splitted#url{fragment=SplitUrl#url.fragment}).

remove_first_segment(Path) ->
    remove_first_segment(Path, true, false).

remove_first_segment([$/ | T], Start, Done) when Start =:= true, Done =:= false ->
    remove_first_segment(T, false, Done);

remove_first_segment([$/ | T], Start, Done) when Done =:= false ->
    remove_first_segment(T, Start, true);

remove_first_segment(Rest, _Start, true) ->
    Rest;

remove_first_segment([H|T], Start, Done) ->
    remove_first_segment(T, false, Done);

remove_first_segment(_, _, false) ->
    [].
        
merge_path(SplitBase, SplitUrl) when SplitBase#url.netloc =/= undefined,
    SplitBase#url.path =:= undefined ->
    "/" ++ SplitUrl#url.path;

merge_path(SplitBase, SplitUrl) ->
    Segments = string:tokens(SplitBase#url.path, "/"),
    WithoutLast = lists:delete(lists:last(Segments), Segments),
    [$/ | string:join(lists:append(WithoutLast, [SplitUrl#url.path]), "/")].
    
remove_dot_segments(Path) ->
    remove_dot_segments(Path, []).

remove_dot_segments([$., $., $/ | Rest], TmpAcc) ->
    %%?debugVal([[$., $., $/ | Rest], TmpAcc]),
    remove_dot_segments(Rest, TmpAcc);

remove_dot_segments([$., $/ | Rest], TmpAcc) ->
    %%?debugVal([[$., $/ | Rest], TmpAcc]),
    remove_dot_segments(Rest, TmpAcc);

remove_dot_segments([$/, $., $/ | Rest], TmpAcc) ->
    %%?debugVal([[$/, $., $/ | Rest], TmpAcc]),
    remove_dot_segments([$/ | Rest], TmpAcc);

remove_dot_segments([$/, $.], TmpAcc) ->
    %%?debugVal([[$/, $.], TmpAcc]),
    remove_dot_segments([], [$/ | TmpAcc]);

remove_dot_segments([$/, $., $., $/ | Rest], []) ->
    %%?debugVal([[$/, $., $., $/ | Rest], here]),
    [$/, $., $., $/ | Rest];

remove_dot_segments([$/, $., $., $/ | Rest], TmpAcc) ->
    %%?debugVal([[$/, $., $., $/ | Rest], TmpAcc]),
    remove_dot_segments([$/ | Rest], remove_first_segment(TmpAcc));

remove_dot_segments([$/, $., $.], TmpAcc) ->
    %%?debugVal([[$/, $., $.], TmpAcc]),
    remove_dot_segments([], [$/ | remove_first_segment(TmpAcc)]);

remove_dot_segments([$., $.], TmpAcc) ->
    %%?debugVal([[$/, $.], TmpAcc]),
    remove_dot_segments([], TmpAcc);

remove_dot_segments([$.], TmpAcc) ->
    %%?debugVal([[$/, $.], TmpAcc]),
    remove_dot_segments([], TmpAcc);

remove_dot_segments([H | Rest], TmpAcc) ->
    %%?debugVal([[H | Rest], TmpAcc]),
    remove_dot_segments(Rest, [H | TmpAcc]);

remove_dot_segments([], TmpAcc) ->
    %%?debugVal(TmpAcc),
    lists:reverse(TmpAcc).

urljoin_(SplitBase, SplitUrl) ->
    SplitUrl#url.abspath.

urlsplit(Url) ->
    urlsplit(Url, get_scheme, #url{abspath=Url}, []).

urlsplit([$:, $/, $/ | Rest], get_scheme, Url, Scheme) when length(Scheme) > 0 ->
    urlsplit(Rest, get_netloc, Url#url{scheme=lists:reverse(Scheme)}, []);
    
urlsplit([$: | Rest], get_scheme, Url, Scheme) when length(Scheme) > 0 ->
    urlsplit(Rest, get_path, Url#url{scheme=lists:reverse(Scheme)}, []);

%% We're here because no ':' was found, but found '//'
urlsplit([$/, $/ | Rest], get_scheme, Url, Scheme) ->
    urlsplit(Rest, get_netloc, Url, []);

urlsplit([$/ | Rest], get_netloc, Url, Netloc) ->
    urlsplit(Rest, get_path, Url#url{netloc=lists:reverse(Netloc)}, [$/]);

urlsplit([$? | Rest], get_netloc, Url, Netloc) ->
    urlsplit(Rest, get_search, Url#url{netloc=lists:reverse(Netloc)}, []);

urlsplit([$# | Rest], get_netloc, Url, Netloc) ->
    urlsplit(Rest, get_path, Url#url{netloc=lists:reverse(Netloc)}, []);

urlsplit([$? | Rest], get_path, Url, Path) when length(Path) > 0 ->
    urlsplit(Rest, get_search, Url#url{path=lists:reverse(Path)}, []);

urlsplit([$? | Rest], get_path, Url, TmpAcc) ->
    urlsplit(Rest, get_search, Url, TmpAcc);

urlsplit([$# | Rest], get_path, Url, Path) when length(Path) > 0 ->
    urlsplit(Rest, get_fragment, Url#url{path=lists:reverse(Path)}, []);

urlsplit([H | T], State, Url, TmpAcc) ->
    %%?debugVal([[H | T], State, Url, TmpAcc]),
    urlsplit(T, State, Url, [H | TmpAcc]);

urlsplit([], get_scheme, Url, _TmpAcc) ->
    urlsplit(Url#url.abspath, get_path, Url, []);

urlsplit([], get_netloc, Url, TmpAcc) ->
    Url#url{netloc=lists:reverse(TmpAcc)};

urlsplit([], get_path, Url, TmpAcc) when length(TmpAcc) > 0 ->    
    Url#url{path=lists:reverse(TmpAcc)};

urlsplit([], get_fragment, Url, TmpAcc) ->
    Url#url{fragment=lists:reverse(TmpAcc)};
    
urlsplit([], get_search, Url, TmpAcc) ->
    Url#url{search=lists:reverse(TmpAcc)};

urlsplit([], _State, Url, _TmpAcc) ->
    Url.

urlunsplit(#url{scheme=Scheme, netloc=Netloc, path=Path, 
    search=Search, fragment=Fragment}) ->
    SchemePlus = if
        Scheme =/= undefined ->
            Scheme ++ ":";
        true ->
            ""
    end,
    NetlocPlus = if
        Netloc =/= undefined ->
            "//" ++ Netloc;
        true ->
            ""
    end,
    PathPlus = if
        Path =/= undefined ->
            Path;
        true ->
            ""
    end,
    QueryPlus = if
        Search =/= undefined ->
            "?" ++ Search;
        true ->
            ""
    end,
    FragmentPlus = if
        Fragment =/= undefined ->
            "#" ++ Fragment;
        true ->
            ""
    end,
    lists:concat([SchemePlus, NetlocPlus, PathPlus, QueryPlus, FragmentPlus]).

urljoin_test_() ->
    
    Base = "http://a/b/c/d",
    
    Tests = [ 
        {"g:h",               "g:h"},                                                                                                                                                            
        {"http:g",            "http://a/b/c/g"},
        {"http:",             "http://a/b/c/d"},
        {"g",                 "http://a/b/c/g"},
        {"./g",               "http://a/b/c/g"},
        {"g/",                "http://a/b/c/g/"},
        {"/g",                "http://a/g"},
        {"//g",               "http://g"},
        {"?y",                "http://a/b/c/d?y"},
        {"g?y",               "http://a/b/c/g?y"},
        {"g?y/./x",           "http://a/b/c/g?y/./x"},
        {".",                 "http://a/b/c/"},
        {"./",                "http://a/b/c/"},
        {"..",                "http://a/b/"},
        {"../",               "http://a/b/"},
        {"../g",              "http://a/b/g"},
        {"../..",             "http://a/"},
        {"../../g",           "http://a/g"},
        {"../../../g",        "http://a/../g"},
        {"./../g",            "http://a/b/g"},
        {"./g/.",             "http://a/b/c/g/"},
        
        % Python tests says result should be "http://a/./g" but I think otherwise.
        % {"/./g",              "http://a/./g"},
        
        {"/./g",              "http://a/g"},        
        {"g/./h",             "http://a/b/c/g/h"},
        {"g/../h",            "http://a/b/c/h"},
        {"http:g",            "http://a/b/c/g"},
        {"http:",             "http://a/b/c/d"},
        {"http:?y",           "http://a/b/c/d?y"},
        {"http:g?y",          "http://a/b/c/g?y"},
        {"http:g?y/./x",      "http://a/b/c/g?y/./x"}
    ],
    
    [?_assertEqual(Expected, urljoin(Base, Url)) || {Url, Expected} <- Tests].
    
urlsplit_test_() ->
    
    Tests = [
        {"http://example.com", 
            #url{scheme="http", netloc="example.com"}},
        {"http://example.com/", 
            #url{scheme="http", netloc="example.com", path="/"}},
        {"http://example.com/?query=true", 
            #url{scheme="http", netloc="example.com", path="/", search="query=true"}},
        {"http://www.ics.uci.edu/pub/ietf/uri/#Related",
            #url{scheme="http", netloc="www.ics.uci.edu", 
                    path="/pub/ietf/uri/", fragment="Related"}},
        {"//example.com", 
            #url{netloc="example.com"}},
        {"?query",
            #url{search="query"}},
        {"g?y",
            #url{path="g", search="y"}},
        {"?query#foo",
            #url{search="query", fragment="foo"}}
    ],
    
    [?_assertEqual(Expected#url{abspath=Url}, urlsplit(Url)) || {Url, Expected} <- Tests],
    
    %% Now handle unsplit
    [?_assertEqual(Url, urlunsplit(urlsplit(Url))) || {Url, _Expected} <- Tests].
    
remove_dot_segments_test_() ->
    Tests = [
        {"", "./"},
        {"/a/g", "/a/b/c/./../../g"},
        {"g/h", "g/./h"},
        {"/../g", "/b/c/../../../g"}
    ],
    
    [?_assertEqual(Expected, remove_dot_segments(Url)) || {Expected, Url} <- Tests].