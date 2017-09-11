
-module(main).
-export([start/0
        ,call_sync_workers/0
        ,call_worker/0
        ,sub_all/0
        ,sub_one/0
        ,stream/0
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
        application:start(node_watch),
        application:start(ppool),
        ppool:start_pool(my, 10, {port_worker, start_link, []}),
        ppool:start_pool(my2, 10, {port_worker, start_link, []}),
        ppool:start_pool(my3, 10, {port_worker, start_link, []}),
        ppool:start_pool(stream, 2, {port_worker, start_link, []}),




        % ppool_worker:start_all_workers(my, "python ./priv/hello.py 2 2"
        %                                    ">> ./logs/hello_err.log"),

        % Cmd = "sudo docker run --rm -i -u python -w /home/python/ -v /tmp/:/tmp/ pyhello:0.1 /usr/bin/python hello.py 2 2>>/tmp/hello.log",

        Cmd = "python ./priv/hello.py 2 2>>/tmp/hello.log",




        Workers = [Cmd,
                   Cmd
                   %%"nodejs ./priv/hello.js 2>> ./logs/hello_err_js.log"
                  ],

        ppool_worker:start_map_workers(my, Workers),
        ppool_worker:start_map_workers(my2, Workers),
        ppool_worker:start_map_workers(my3, Workers),





        
        %for(10, fun() -> 
        %                ppool_worker:call_workers(my, "{\"in\":2}\n")
        %        end
        %),
    

        ok.


stream() ->
        Workers = ["python ./priv/hello_stream.py 1 2>> ./logs/hello_err.log",
                   "python ./priv/hello_stream.py 2 2>> ./logs/hello_err.log"
                   %%"nodejs ./priv/hello.js 2>> ./logs/hello_err_js.log"
                  ],

        ppool_worker:start_map_workers(stream, Workers),


    ppool_worker:stream_all_workers(stream, "{\"in\":2}\n"). 


call_sync_workers() ->
    ppool_worker:call_sync_workers(my, "{\"in\":2}\n").


call_worker() ->
    ppool_worker:call_worker(my, "{\"in\":2}\n").



sub_all() ->
    ppool_worker:subscribe(my, my3, <<"in\":2">>, all),
    ppool_worker:call_worker(my, "{\"in\":2}\n").

sub_one() ->
    ppool_worker:subscribe(my, my2, no, one),
    ppool_worker:call_worker(my, "{\"in\":2}\n").





 

