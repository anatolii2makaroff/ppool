
-module(main).
-export([
         start/0
        ]).


start() ->
    application:start(node_watch),
    application:start(ppool),
    application:start(node_scheduler),
 
       ok.


