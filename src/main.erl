

-module(main).
-export([
         start/0,
         stop/0
        ]).


start() ->
    application:start(ppool),
    application:start(node_scheduler),
    application:start(node_watch),
 
       ok.

stop() ->
    application:stop(node_watch),
    application:stop(node_scheduler),
    application:stop(ppool),

       ok.


