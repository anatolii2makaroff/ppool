
-module(main).
-export([start/0]).

start() ->
    application:start(ppool),
    ppool:start_pool(my, 10, {worker, start_link, [my]}),
    ppool_worker:start_worker(my),
    ppool_worker:start_worker(my),
    ppool_worker:start_worker(my),


    ppool_worker:call_all_workers(my, 1000),
    ppool_worker:call_all_workers(my, 1000),

    ppool_worker:call_all_workers(my, 1000),
    ppool_worker:call_all_workers(my, 1000),

    ppool_worker:call_all_workers(my, 1000),
    ppool_worker:call_all_workers(my, 1000),

    ppool_worker:call_all_workers(my, 1000),
    ppool_worker:call_all_workers(my, 1000),


    ppool_worker:call_all_workers(my, 1000),
    ppool_worker:call_all_workers(my, 1000),

    ppool_worker:call_all_workers(my, 1000),
    ppool_worker:call_all_workers(my, 1000),



    ok.

