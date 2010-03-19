#!/usr/bin/env escript
%% -*- erlang -*-

% ErlangUserConference2009-WienerandGoldberg.pdf
% Anonymity in Erlang (self-applications)

main(_) ->

    % recursion-maker/fixed-point combinator

    Y1 =
	fun (F) ->
		(fun (X) -> X(X) end)
		  (fun (M) -> F(fun (Arg) -> (M(M))(Arg) end) end)
	end,

    % factorial function as self-application

    Fact0 = fun (N) ->
		    (fun (F) -> F(F, N) end)
		      (fun (F, N2) ->
			       case N2 of
				   0 -> 1;
				   _ -> N2 * F(F, N2 - 1)
			       end
		       end)
	    end,
    
    % factorial function made from a combinator
    
    Fact1 =
	Y1 (fun (F) ->
		    fun (N) ->
			    case N of
				0 -> 1;
				_ -> N * F(N - 1)
			    end
		    end
	    end),
    
    %

    io:format("~p~n", [[
			Fact0(6),
			Fact1(6)
		      ]]),

    ok.
