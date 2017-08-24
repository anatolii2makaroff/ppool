-module(ppool_worker_state).
%-behaviour(gen_statem).

%-export([start/0,push/0,get_count/0,stop/0]).
%-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%-export([on/3,off/3]).


%callback_mode() -> handle_event_function.

