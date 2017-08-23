
-module(main).
-export([start/0]).



for(N, F)
    when N>0->
    F(),
    if
        N=:=-9 -> % ppool_worker:cast_all_workers(my,10000),
                  timer:sleep(2000);
        true -> ok
    end,

    for(N-1, F);

for(0, _) ->
    ok.


start() ->
    spawn(
      fun() ->
        application:start(ppool),
        ppool:start_pool(my, 10, {port_worker, start_link, []}),
        ppool_worker:start_all_workers(my, "python ./priv/hello.py 2"),
        
        for(10, fun() -> 
                        ppool_worker:call_all_workers(my, "{'in':1}\n")
                end
        ),
    

        ok

      end).

