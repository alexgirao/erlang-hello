doc/design_principles/part_frame.html

call vs cast: call returns, cast not

ch1.erl: stand-alone server, exports: init/0, alloc/0, free/1
ch2.erl: decoupled server, mimics gen_server, exports: init/0, handle_call/2, handle_cast/2
ch3.erl: gen_server usage, exports: init/1, handle_call/3, handle_cast/2, terminate/2
code_lock.erl: gen_fsm usage, exports: init/1, terminate/3, handle_event/3, locked/2, open/2
