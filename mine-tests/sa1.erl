#!/usr/bin/env escript
%% -*- erlang -*-

% ErlangUserConference2009-WienerandGoldberg.pdf
% Anonymity in Erlang (self-applications)

main(_) ->

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
    
    % recursion-maker/fixed-point combinator

    Y1 =
	fun (F) ->
		(fun (G) -> G(G) end)
		  (fun (G) ->
			   % F/F0 is called with a closure (H) and returns F1
			   % the first and subsequent calls to F1 will recurse on H
			   H = fun (A) ->
				       %io:format("~p~n", [A]),
				       (G(G))(A)
			       end,
			   F(H)
		   end)
	end,
    
    % factorial function made from a combinator
    
    Fact1 = Y1(
	      fun (F) ->                           % F0
		      fun (N) ->                   % F1
			      case N of
				  0 -> 1;
				  _ -> N * F(N - 1)   % H(A)
			      end
		      end
	      end
	     ),
    
    %
    
    io:format("~p~n", [[
			Fact0(6),
			Fact1(6)
		      ]]),

    ok.
