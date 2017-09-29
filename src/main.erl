
-module(main).
-export([start/0
        ,cmd/3
        ,start_all_workers/0
        ,start_stream_workers/0
        ,start_map_workers/0
        ,call_sync_workers/0
        ,call_worker/0
        ,stream/1
        ,sub_all/0
        ,sub_one/0

        ,start_distrib/1
        ,call_distrib/1
        ]).


start() ->
    application:start(node_watch),
    application:start(ppool),
    application:start(node_scheduler),
 
    ppool:start_pool(ppool, {python, 10, 30000, {port_worker, start_link, []} }),
    ppool:start_pool(ppool, {nodejs, 10, 30000, {port_worker, start_link, []} }),
    ppool:start_pool(ppool, {python_stream, 2, 30000,
                            {port_worker, start_link, []} }),

    ppool:start_pool(ppool, {w2, 10, 30000, {worker, start_link, []} }),
 
        ok.


start_all_workers() ->
    ppool_worker:start_all_workers(python, cmd("hellopy:0.1.0",
                                               "python hello.py 1",
                                               "hello.log")).


start_stream_workers() ->
    ppool_worker:start_all_workers(python_stream, cmd("hellopy:0.1.0",
                                               "python hello_stream.py 1",
                                               "hello_stream.log")).



start_map_workers() ->

    Cmds=[cmd("hellojs:0.1.0","nodejs hello.js", "hellojs.log")
         ,cmd("hellojs:0.1.0","nodejs hello.js", "hellojs.log")
         ,cmd("hellojs:0.1.0","nodejs hello.js", "hellojs.log")
         ],

    ppool_worker:start_map_workers(nodejs, Cmds).


call_sync_workers() ->
    ppool_worker:call_sync_workers(python, "hello python\n").


call_worker() ->
    ppool_worker:call_worker(python, "hello python\n").



stream(M) ->
    ppool_worker:stream_all_workers(python_stream, M++"\n").


sub_all() ->
    ppool_worker:subscribe(python_stream, nodejs, <<"start">>, all).


sub_one() ->
    ppool_worker:subscribe(python_stream, python, no, one).


%% distribute
%%


start_distrib(T) ->
    node_scheduler:call(T, fun ppool:start_pool/2, ppool, 
                        {test2, 10, {port_worker, start_link, []}} ),

    node_scheduler:call(T, fun ppool_worker:start_all_workers/2, test2, 
                         cmd("hellopy:0.1.0", 
                             "python hello.py 1", 
                             "hello.log")
                       ).


call_distrib(T) ->
    node_scheduler:call(T, fun ppool_worker:call_workers/2, 
                        test2, "asdasda\n" ).



cmd(Img, Cmd, L) ->
   R = lists:concat(["sudo docker run --rm -i -u drop -w /home/drop/"
                     " -v /tmp/:/tmp/ ", Img, " ", Cmd, 
                     " 2>>/tmp/", L]),
   R.


for(N, F)
    when N>0->
    F(),

    for(N-1, F);

for(0, _) ->
    ok.

