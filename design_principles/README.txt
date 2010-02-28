doc/design_principles/part_frame.html

call vs cast: call returns, cast not

ch1.erl: stand-alone server, exports: init/0, alloc/0, free/1
ch2.erl: decoupled server, mimics gen_server, exports: init/0, handle_call/2, handle_cast/2
ch3.erl: gen_server usage, exports: init/1, handle_call/3, handle_cast/2, terminate/2
code_lock.erl: gen_fsm usage, exports: init/1, terminate/3, handle_event/3, locked/2, open/2
file_logger.erl: gen_event event handler
terminal_logger.erl: gen_event event handler

gen_event test

    gen_event:start_link({local, man01}).
    gen_event:add_handler(man01, terminal_logger, []).
    gen_event:notify(man01, no_reply).
    gen_event:notify(man01, "something").
    gen_event:delete_handler(man01, terminal_logger, []).
    gen_event:notify(man01, no_reply).
    gen_event:stop(man01).
