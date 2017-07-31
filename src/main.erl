
-module(main).
-export([start/0]).

start() ->
    application:start(ppool),
    ppool:start_pool(my, 10, {worker, start_link, []}),

    ppool_worker:run(my, 1000).
