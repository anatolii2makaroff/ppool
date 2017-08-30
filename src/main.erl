
-module(main).
-export([start/0
        ,call_all_workers/0
        ]).



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

        % ppool_worker:start_all_workers(my, "python ./priv/hello.py 2 2"
        %                                    ">> ./logs/hello_err.log"),



        Workers = ["python ./priv/hello.py 1 2>> ./logs/hello_err.log",
                   "python ./priv/hello.py 2 2>> ./logs/hello_err.log",
                   "node ./priv/hello.js 2>> ./logs/hello_err_js.log"
                  ],

        ppool_worker:start_map_workers(my, Workers),



        
        for(1, fun() -> 
                        ppool_worker:call_all_workers(my, "{\"in\":2}\n")
                end
        ),
    

        ok

      end).


call_all_workers() ->
    ppool_worker:call_all_workers(my, "{\"in\":2}\n").


