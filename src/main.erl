
-module(main).
-export([
         start/0,
         stop/0
        ]).


start() ->
    application:start(node_watch),
    application:start(ppool),
    application:start(node_scheduler),
 
       ok.

stop() ->
    application:stop(node_watch),
    application:stop(node_scheduler),
    application:stop(ppool),

       ok.


