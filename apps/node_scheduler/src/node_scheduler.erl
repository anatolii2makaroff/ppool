-module(node_scheduler).
-behaviour(gen_server).

%% API.
-export([start_link/0,
         call/4,
         cmd/3,
         api/1,
         node_info_internal_stream/1,
         try_start/1,
         restart/0
         
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).


-include_lib("stdlib/include/ms_transform.hrl").

-include("node_scheduler.hrl").
-include("../../src/ppool.hrl").



%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server.

init([]) ->
	{ok, #state{}, 0}.


restart() ->
    gen_server:call(?MODULE, restart).


handle_call(restart, _From, State) ->

    ppool:stop_pool(ppool, node_info_stream),
    ppool:stop_pool(ppool, node_info_internal_stream),
    ppool:stop_pool(ppool, node_collector),
    ppool:stop_pool(ppool, rrd),
    ppool:stop_pool(ppool, node_api),
    ppool:stop_pool(ppool, flower),
    ppool:stop_pool(ppool, flower_sc_stream),
    ppool:stop_pool(ppool, webbone),

	{reply, ok, State, 0};


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info(timeout, State) ->

    %% Systems pools

    ppool:start_pool(ppool, {node_info_stream, ?NODE_INFO_WORKERS, 
                            {port_worker, start_link, []} }),

    ppool:start_pool(ppool, {node_collector, ?NODE_CLTR_WORKERS, 
                            {port_worker, start_link, []} }),

    ppool:start_pool(ppool, {rrd, ?NODE_RRD_WORKERS, 
                            {port_worker, start_link, []} }),

    ppool:start_pool(ppool, {node_api, ?NODE_API_WORKERS, 
                            {worker, start_link, []} }),

    ppool:start_pool(ppool, {node_info_internal_stream, ?NODE_INFO_IN_WORKERS, 
                            {worker, start_link, []} }),

    ppool:start_pool(ppool, {flower, ?FLOWER_WORKERS, 
                            {port_worker, start_link, []} }),

    ppool:start_pool(ppool, {flower_sc_stream, ?FLOWER_SC_WORKERS, 
                            {port_worker, start_link, []} }),

    ppool:start_pool(ppool, {webbone, ?WEBBONE_WORKERS, 
                            {port_worker, start_link, []} }),



    %% link ppools
    
    ppool_worker:subscribe(node_info_stream, {node_collector, <<"no">>, dall}),

    ppool_worker:subscribe(node_info_internal_stream, {node_collector, <<"no">>, dall}),

    ppool_worker:subscribe(node_collector, {rrd, <<"_trace">>, sone}),

    ppool_worker:subscribe(flower, {node_api, <<"system::">>, one}),

    ppool_worker:subscribe(flower_sc_stream, {flower, <<"system::">>, sone}),


    %% System info stream worker

    ppool_worker:start_worker(node_info_stream,         
               {lists:concat([os:getenv("DROP_HOME"),
                              "/priv/node_info_stream/node_info_stream ",
                              node(), " ",
                              ?NODE_INFO_INTERVAL,
                              " 2>>",
                              os:getenv("DROP_LOG_DIR"),
                              "/node_info_stream.log"
                             ]), ?NODE_INFO_TIMEOUT}
    ),

    %% Collect info from all nodes node_info_stream 

    %% master 
    ppool_worker:start_worker(node_collector, 
                              {cmd("-m 300m node_collector:"?NODE_CLTR_VER,
                                   "./node_collector /tmp/db 1000 5 ",
                                   "node_collector.log"
                                  ), ?NODE_CLTR_TIMEOUT}
    ),


    %% rrd

    ppool_worker:start_all_workers(rrd, 
                              {cmd("-m 100m rrd:"?NODE_RRD_VER,
                                   "./rrd /tmp/rrd ",
                                   "rrd.log"
                                  ), ?NODE_RRD_TIMEOUT}
    ),


    %% api

    ppool_worker:start_all_workers(node_api, 
                              {{node_scheduler, api}, ?NODE_API_TIMEOUT}
    ),

    ppool_worker:start_worker(node_info_internal_stream, 
                              {{node_scheduler, node_info_internal_stream}, ?NODE_INFO_IN_TIMEOUT}
    ),

    %% flower

    ppool_worker:start_all_workers(flower, 
                              {cmd("-m 100m flower:"?FLOWER_VER,
                                   "./flower /tmp/flows/ ",
                                   "flower.log"
                                  ), ?FLOWER_TIMEOUT}
    ),

    %% scheduler

    ppool_worker:start_worker(flower_sc_stream,         
               {lists:concat([os:getenv("DROP_HOME"),
                              "/priv/flower_sc_stream/flower_sc_stream ",
                              os:getenv("DROP_VAR_DIR"), " ", node(), " ",
                              ?FLOWER_SC_INTERVAL, " ",
                              os:getenv("DROP_VIP"), " ", os:getenv("DROP_VIP_IFACE"),
                              " 2>>",
                              os:getenv("DROP_LOG_DIR"),
                              "/flower_sc_stream.log"
                             ]), ?FLOWER_SC_TIMEOUT}
    ),


    %% webbone

    ppool_worker:start_worker(webbone,         
               {lists:concat(["docker run --rm -i -u root -w /home/drop/",
                 " --net=host -v /var/lib/drop:/var/lib/drop",
                 " -v /var/log/nginx:/var/log/nginx -e GROUP=webbone -m 100m webbone:"?WEBBONE_VER,
                 " /bin/bash start.sh ", os:getenv("DROP_VIP"), " 2>/dev/null"
                ]), ?WEBBONE_TIMEOUT}
    ),


	  {noreply, State};



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



cmd(Img, Cmd, Log) ->

  [Gr,_] = string:split(lists:last(string:split(Img, " ", all)), ":"),

   R = lists:concat(["docker run --rm --log-driver none -i -u drop -w /home/drop/"
                     " -v ", os:getenv("DROP_VAR_DIR"), ":/tmp ", 
                     "-e GROUP=", Gr, " ", Img, " ", Cmd, 
                     " 2>>", os:getenv("DROP_LOG_DIR"), "/", Log]),
   R.


try_start(N) ->
     case N() of
          {ok, []} -> 
             timer:sleep(1000),
             try_start(N);

        _ -> ok

     end.



call(Type, F, Name, Cmd) ->

    ?Debug2({?MODULE, Type, F, Name, Cmd}),


    Tf = fun(P0, Cmd0) ->
            try F(P0, Cmd0) of
                _ -> ok
            catch 
                _:_ -> 
                     error_logger:warning_msg("call api ~p~n ",
                                              [Cmd]),
                      ok
            end
        end,


    Get_memb = fun(N0, F0) ->
                      try F0(N0) of
                          {error,{no_such_group,N0}} ->
                             [];

                            R -> R
                      catch 
                         _:_ ->

                           error_logger:warning_msg("get members api ~p~n ",
                                              [N0]),
                            []
            end
        end,


    case Type of

        local ->
            [Tf(P, Cmd)||P <- [X || X<- Get_memb(Name, fun pg2:get_members/1), 
                                                   node(X)=:=node()]];

        near ->
            [Tf(P, Cmd)||P <- [ Get_memb(Name, fun pg2:get_closest_pid/1)]];

        all ->
            [Tf(P, Cmd)||P <-  Get_memb(Name, fun pg2:get_members/1)];
 
        Node ->
            [Tf(P, Cmd)||P <- [X || X <- Get_memb(Name, fun pg2:get_members/1), node(X)=:=Node]]
 
    end.


%%
%%
%% INFO stream worker 
%% tick every X secs
%%  
%%


average([]) ->
    0;

average(X) ->
        lists:sum(X) / length(X).



get_worker_info(N) ->


  R=ets:select(N,
                ets:fun2ms(fun(X)
                                -> X 
                            end)
              ),
  
   Err=length([X||X <- R, X#worker_stat.status =:=error]),
    Touts=length([X||X <- R, X#worker_stat.status =:=timeout]),
      Run=length([X||X <- R, X#worker_stat.status =:=running]),
       Cnt=length([X||X <- R, X#worker_stat.status =:=ok]),
 
      Elaps=average([timer:now_diff(X#worker_stat.time_end, 
                                    X#worker_stat.time_start)
                     ||X <- R, X#worker_stat.time_end=/=undefined]),

       ["system::", 
        atom_to_list(node()), "::",
        atom_to_list(N), "::",
        integer_to_list(Err), "::",
        integer_to_list(Touts), "::",
        integer_to_list(Run), "::",
        integer_to_list(Cnt), "::",
        lists:flatten(io_lib:format("~p", [Elaps]))
       ].



node_info_loop(F) ->

  %% ppools error & timeout
  %%
   
  List=[X||X<-pg2:which_groups(), 
            lists:suffix("_ev", atom_to_list(X))=/=true,
            X=/=ppool
       ],

   lists:foreach(fun(M)->

                      try get_worker_info(M) of
                        Msg -> 
                            Msg2=list_to_binary(Msg),
                                F!{self(), {data, [Msg2]}}
 
                      catch
                        _:_ -> ok
                      end  
                end,
                 List

   ),

    timer:sleep(?NODE_INFO_IN_TICK),
      node_info_loop(F).


node_info_internal_stream(F) ->
    receive
        _ -> 
          ?Debug2({start_node_in_worker, F}), 
            node_info_loop(F)
    end.



%%
%%
%%
%% API worker
%%
%%

call_api(Msg) ->

           ?Debug3({recv, Msg}),

            R = binary:replace(Msg, <<"\n">>, <<>>),

            [_|[Tp|[Fn|[Name|A]]]] = binary:split(R, <<"::">>, [global]),

           ?Debug3({Tp, Fn, Name, A}),

           case erlang:binary_to_atom(Fn, latin1) of

               start_pool ->

                   [Cnt|_] = A,

                    _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, {Nm, C}) -> 
                                    ppool:start_pool(N, 
                                                     {Nm, C, 
                                                      {port_worker, start_link, []}
                                                     }
                                                    ) end,

                            ppool, 
                            {erlang:binary_to_atom(Name, latin1),
                             erlang:binary_to_integer(Cnt)}
                              );


               stop_pool ->
                   
                    _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> ppool:stop_pool(N, C) end,
                            ppool,
                            erlang:binary_to_atom(Name, latin1)
                             );


               start_worker ->

                   [Img, Cmd, Log, Tm] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, {I, C, L, T}) -> ppool_worker:start_worker(N, 
                                             {cmd(I, C, L), T})
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            {
                             erlang:binary_to_list(Img),
                             erlang:binary_to_list(Cmd),
                             erlang:binary_to_list(Log),
                             erlang:binary_to_integer(Tm)
                            }
                             );

               stop_worker ->


                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, _C) -> ppool_worker:cast_worker(N, 
                                                    stop)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            false
                             );


               stop_all_workers ->

                   [Cnt|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                     ppool_worker:stop_all_workers(N, C)

                            end,

                            erlang:binary_to_atom(Name, latin1),
                            erlang:binary_to_integer(Cnt)
                             );


               start_all_workers ->


                   [Img, Cmd, Log, Tm] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, {I, C, L, T}) -> 
                                    ppool_worker:start_all_workers(N, 
                                                                   {cmd(I, C, L), 
                                                                    T})
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            {
                             erlang:binary_to_list(Img),
                             erlang:binary_to_list(Cmd),
                             erlang:binary_to_list(Log),
                             erlang:binary_to_integer(Tm)
                            }
                             );


               call_worker ->

                   [Args|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                     ppool_worker:call_worker(N, C)

                            end,

                            erlang:binary_to_atom(Name, latin1),
                            <<Args/binary, <<"\n">>/binary>>
                             );

               cast_worker ->

                   [Args|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:cast_worker(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            <<Args/binary, <<"\n">>/binary>>
                             );


               first_call_worker ->

                   [Args|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:first_call_worker(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            <<Args/binary, <<"\n">>/binary>>
                             );


               change_limit ->

                   [Cnt|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:change_limit(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            erlang:binary_to_integer(Cnt)
                           
                             );

               call_workers ->

                   [Args|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:call_workers(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                             <<Args/binary, <<"\n">>/binary>>
                             );
                

               cast_all_workers ->

                   [Args|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:cast_all_workers(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                             <<Args/binary, <<"\n">>/binary>>
                             );


               stream_all_workers ->

                   [Args|_] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:stream_all_workers(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                             <<Args/binary, <<"\n">>/binary>>
                             );


               subscribe ->

                   [To, Fl, Api] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:subscribe(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                             {erlang:binary_to_atom(To, latin1),
                              Fl,
                              erlang:binary_to_atom(Api, latin1)
                              }
                             );


               unsubscribe ->

                   [To] = A,

                   _Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:unsubscribe(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                             {erlang:binary_to_atom(To, latin1)
                             }
                             );


               _M ->
                 ok,
                  ?Debug3({unknow_call_api, _M})

           end,

           ok.




api(F) ->
    receive

         Msg ->

            Msgs = binary:split(Msg, ?SPLIT_MSG_SEQ, [global]),
             ?Debug3({recv, Msgs}),

             lists:foreach(fun(M) -> call_api(M) end,  Msgs),

               F!{self(), {data, [<<"ok">>]}},

             api(F)


    end.
