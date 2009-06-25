
- 'man 3 erlang' for BIFs (Built-In Functions) documentation
- or erl5.7.1/erts-5.7.1/doc/html/erlang.html

tuples.escript

    - how to use tuples

flush-unknown.erl / flush-unknown.escript

	- how basic messaging works
	- how do flush unknown messages and avoid filling up message queues
	- ?MODULE macro
	- self/0 BIF
	- spawn/1 BIF
	- spawn/3 BIF
	- using non-standard module names ('flush-unknown')

	reference: programming_rules.pdf

echo.erl / echo.escript

	- register/2 BIF
	- whereis/1 BIF
	- fun Name/Arity usage

    reference: erl5.7.1/doc/reference_manual/part_frame.html

arguments.escript

	- how to handle arguments with erlang's escript

sum.erl

	shows differences between recursive calls and tail calls

	reference: c4-1_erlang.zip

lc.erl

	- list comprehensions
    - fun vs fun expression

	reference: http://erlang-python-java.blogspot.com/2008/01/little-erlange-exercise-1.html
    reference: erl5.7.1/doc/reference_manual/part_frame.html

fib.erl

	simple guard (when) test

fib2.erl / fib2.py

	solving the problem posed in a simpler manner, problem: find
        the sum of all the even-valued terms in the fibonacci sequence
        which do not exceed one million.

	reference: http://erlang-python-java.blogspot.com/2008/01/little-erlange-exercise-2.html

records.erl / records.escript

	- records usage

tt.erl / tt.escript

       - timer:tc usage

       - reference: http://localhost/doc/erlang-doc-html/html/doc/efficiency_guide/part_frame.html
