
-module(main).
-export([start/0]).



for(N, F)
    when N>0->
    F(),
    if
        N=:=2 -> timer:sleep(2000);
        true -> ok
    end,

    for(N-1, F);

for(0, _) ->
    ok.


start() ->
    application:start(ppool),
    ppool:start_pool(my, 10, {worker, start_link, []}),
    ppool_worker:start_worker(my),
    ppool_worker:start_worker(my),
    ppool_worker:start_worker(my),

    for(12, fun() -> 
                    ppool_worker:call_all_workers(my, 100)
            end
    ),
 

    ok.

