
- 'man 3 erlang' for BIFs (Built-In Functions) documentation
- or erl5.7.1/erts-5.7.1/doc/html/erlang.html

varbinding.escript

	- use of the match operator (=)
    	- variable binding and pattern matching with tuples, lists and records

define.escript

	- how to define constants

tuples.escript

	- how to use tuples
    	- 'import' directive
    	- append_element/2

f.erl

	- how to use 'erlc' and 'erl' to run programs non-interactively
	- basic unit testing usage
	- head/tail list processing
	- recursive vs tail recursive

pingpong.erl

    - how to create processes (spawn/3 BIF)
    - how basic messaging works

flush-unknown.erl / flush-unknown.escript

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
	- registered/0
	- erlang:yield/0

    	reference: erl5.7.1/doc/reference_manual/part_frame.html

p.erl

	- $ erlc p.erl && erl -noshell -s p main -s init stop
	- basic process management
	- processes/0
	- process_info/1 and process_info/2
	- whereis/1
	- registered/0
	- erlang:start_timer/3
	- process_flag(trap_exit, true)
	- exit/2

arguments.escript

	- how to handle arguments with erlang's escript

format.escript

	- how to format a string to a variable

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
	- how to use record_info/2 BIF

tt.erl / tt.escript / tt.txt

       - timer:tc usage
       - test_avg function
       - 'import' directive usage

       - reference: http://localhost/doc/erlang-doc-html/html/doc/efficiency_guide/part_frame.html
       - reference: http://www.trapexit.org/Measuring_Function_Execution_Time

l.erl

	- usage of lists module
	- $ erl -noshell -s l main -s init stop

	- reference: http://localhost/doc/erlang-doc-html/html/lib/stdlib-1.15.3/doc/html/index.html
	- reference: http://localhost/doc/erlang-doc-html/html/lib/stdlib-1.15.3/doc/html/lists.html

if.erl

	- $ erl -noshell -s if main -s init stop
	- short-circuit expressions, 'andalso' and 'orelse' operators

urlutil.erl

    - basic string manipulation

    - reference: http://userprimary.net/user/2009/02/09/learning-string-manipulation-in-erlang/

reverse_test.erl

    - simple unit testing

    - reference: reverse_test.txt

http_util_elias.erl

    - heavy unit testing
    - heavy string manipulation
    - how to export all functions in a module, directive
      "-compile(export_all)"

    - reference: http_util_elias.txt

filetest.erl

    - read from a file with file:read_file/1
    - write to a file with file:write_file/2

sa.erl / sa1.erl

    - self-application/fixed-point combinator/closures

macro.erl / macro.hrl

    - defines
    - macro definition
    - library inclusion
    - error_logger module usage

base36.escript

    - integer arithmetic
    - lists:keyfind

ls.escript

    - iolist_to_binary/1 usage
    - how lists are serialized in [Term1,...,TermN] (shorthand) and [Term1|[...|[TermN|[]]]] (canonical) forms
    - proper vs improper lists
    - native 32-bit integer as unsigned big-endian 4-byte stream (c's htonl function)

sha1.escript

    - sha1 calculation using libcrypto
    - bignum (160-bit) to big-endian binary

join.escript / string_join.erl

    - how to join a list with a separator

se.escript

    - how to side effect

ls2.escript

    - list_ext (108) vs string_ext (107)

dicttest.escript

    - how to use dict module
    - how to use catch expression

mcast0.erl

    - udp networking
    - multicast listening

gcd.escript

    - greatest common denominator (GCD)
    - float to integer (erlang:trunc/1)
    - integer and float formatting (io:format/2)

gb_trees0.erl

    - how to use gb_trees module
    - gb_trees is typically faster than dict for large sets (> 10k),
      see ../articles/025277.html, it also has the ordered keys
      feature

loop-state.erl

    - how to manage state in processes
