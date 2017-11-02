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



    %% link ppools
    
    ppool_worker:subscribe(node_info_stream, {node_collector, <<"no">>, dall}),
    ppool_worker:subscribe(node_info_internal_stream, {node_collector, <<"no">>, dall}),

    ppool_worker:subscribe(node_collector, {rrd, <<"_trace">>, one}),

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
                              {cmd("node_collector:"?NODE_CLTR_VER,
                                   "./node_collector /tmp/db 100 5 ",
                                   "node_collector.log"
                                  ), ?NODE_CLTR_TIMEOUT}
    ),


    %% rrd

    ppool_worker:start_all_workers(rrd, 
                              {cmd("rrd:"?NODE_RRD_VER,
                                   "./rrd /tmp/rrd ",
                                   "rrd.log"
                                  ), ?NODE_RRD_TIMEOUT}
    ),


    %% api

    ppool_worker:start_all_workers(node_api, 
                              {{node_scheduler, api}, ?NODE_API_TIMEOUT}
    ),

    ppool_worker:start_all_workers(node_info_internal_stream, 
                              {{node_scheduler, node_info_internal_stream}, ?NODE_INFO_IN_TIMEOUT}
    ),


     % try_start(node_info_stream),

	  {noreply, State};



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



cmd(Img, Cmd, Log) ->
   R = lists:concat(["docker run --rm -i -u drop -w /home/drop/"
                     " -v ", os:getenv("DROP_VAR_DIR"), ":/tmp ", 
                     Img, " ", Cmd, 
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

    case Type of

        near ->
            [F(P, Cmd)||P <- [pg2:get_closest_pid(Name)]];

        all ->
            [F(P, Cmd)||P <- pg2:get_members(Name)];
 
        dyn ->
            %% TODO dynamic ppool choose
            ok;

        Node ->
            [F(P, Cmd)||P <- [X || X<-pg2:get_members(Name), node(X)=:=Node]]
 
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
            string:find(atom_to_list(X), "_ev")=:=nomatch,
            X=/=ppool
       ],

   lists:foreach(fun(M)->
                      Msg=list_to_binary(get_worker_info(M)),
                       ?Debug2(Msg),
                        
                        F!{self(), {data, [Msg]}}
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

api(F) ->
    receive

        R ->
            [_|[Tp|[Fn|[Name|A]]]] = binary:split(R, <<"::">>, [global]),

           ?Debug2({Tp, Fn, Name, A}),

           case erlang:binary_to_atom(Fn, latin1) of

               start_pool ->

                   [Cnt|_] = A,

                    Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, {Nm, C}) -> 
                                    ppool:start_pool(N, 
                                                     {Nm, C, 
                                                      {port_worker, start_link, []}
                                                     }
                                                    ) end,

                            ppool, 
                            {erlang:binary_to_atom(Name, latin1),
                             erlang:binary_to_integer(Cnt)}
                              ),

                     F!{self(), {data, {response, F, Res}}};

               stop_pool ->
                   
                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> ppool:stop_pool(N, C) end,
                            ppool,
                            erlang:binary_to_atom(Name, latin1)
                             ),
                    F!{self(), {data, {response, F, Res}}};



               start_worker ->

                   [Img, Cmd, Log, Tm] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
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
                             ),

                    F!{self(), {data, {response, F, Res}}};


               stop_workers ->


                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, _C) -> ppool_worker:cast_all_workers(N, 
                                                    stop)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            false
                             ),

                    F!{self(), {data, {response, F, Res}}};



               start_all_workers ->


                   [Img, Cmd, Log, Tm] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
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
                             ),

                    F!{self(), {data, {response, F, Res}}};


               call_worker ->

                   [Args|_] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:call_worker(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            [Args] ++ "\n"
                             ),

                    F!{self(), {data, {response, F, Res}}};


               call_workers ->

                   [Args|_] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:call_workers(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            [Args] ++ "\n"
                             ),

                    F!{self(), {data, {response, F, Res}}};

               cast_all_workers ->

                   [Args|_] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:cast_all_workers(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            [Args] ++ "\n"
                             ),

                    F!{self(), {data, {response, F, Res}}};


 
               stream_all_workers ->

                   [Args|_] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:stream_all_workers(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                            [Args] ++ "\n"
                             ),

                    F!{self(), {data, {response, F, Res}}};



               subscribe ->

                   [To, Fl, Api] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:subscribe(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                             {erlang:binary_to_atom(To, latin1),
                              Fl,
                              erlang:binary_to_atom(Api, latin1)
                              }
                             ),

                    F!{self(), {data, {response, F, Res}}};


               unsubscribe ->

                   [To] = A,

                   Res = call(erlang:binary_to_atom(Tp, latin1),
                            fun(N, C) -> 
                                    ppool_worker:unsubscribe(N, C)
 
                            end,
                            erlang:binary_to_atom(Name, latin1),
                             {erlang:binary_to_atom(To, latin1)
                             }
                             ),

                    F!{self(), {data, {response, F, Res}}}

 
           end,

            
             api(F)

    end.
