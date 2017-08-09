
-module(main).
-export([start/0]).

start() ->
    application:start(ppool),
    ppool:start_pool(my, 10, {worker, start_link, []}),
    ok.
    %    ppool_worker:start_worker(my, 1000).
