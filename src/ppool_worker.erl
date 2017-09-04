-module(ppool_worker).
-behaviour(gen_server).

%% API.
-export([start_link/3,
         start_worker/2,
         register_worker/2,
         start_all_workers/2,
         start_map_workers/2,
         stop_all_workers/1,


         call_worker/2,
         call_map_workers/2,
         call_workers/2,
         call_sync_workers/2,
         cast_all_workers/2,

         get_result_worker/2
         
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-include("ppool.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {
          limit=10, 
          mfa, 
          name,
          workers_pids=[]
          
}).


%% init

start_link(Name, Limit, MFA) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name, Limit, MFA], []).


init([Name, Limit, MFA]) ->
    Name = ets:new(Name, [set, public, named_table, 
                          {keypos, #worker_stat.pid}]),
	{ok, #state{limit=Limit, mfa=MFA, name=Name}}.

register_worker(Name, Pid) ->
    gen_server:call(Name, {register, Pid}).


%% start/stop worker

start_worker(Name, Cmd) ->
    gen_server:call(Name, {start_worker, Cmd}).

start_all_workers(Name, Cmd) ->
    case start_worker(Name, Cmd) of 

       full_limit -> {ok, full_limit};
        _ -> start_all_workers(Name, Cmd)

    end.

start_map_workers(Name, [Cmd|T]) ->
    case start_worker(Name, Cmd) of 

       full_limit -> {ok, full_limit};
        _ -> start_map_workers(Name, T)

    end;

start_map_workers(_Name, []) ->
    ok.


stop_all_workers(Name) ->
    gen_server:call(Name, {stop_all_workers}).


%% API

call_worker(Name, Msg) ->
    gen_server:call(Name, {call_worker, {msg, Msg}}).


call_map_workers(Name, Msg) ->
    lists:map(fun(M) -> call_worker(Name, M) end, 
                                   Msg).
	

call_sync_workers(Name, Msg) ->
    gen_server:call(Name, {call_workers, {sync_msg, Msg}}).

call_workers(Name, Msg) ->
    gen_server:call(Name, {call_workers, {msg, Msg}}).



cast_all_workers(Name, Msg) ->
    gen_server:cast(Name, {cast_all_workers, {msg, Msg}}).


get_result_worker(Name, Msg) ->
    gen_server:call(Name, {get_result_worker, Msg}).


%% callbacks


handle_call({start_worker, Cmd}, _From, #state{name=Name, 
                                               limit=Limit
                                         }=State ) 
  when Limit > 0 ->

    NewLimit = Limit - 1,

         {ok, Pid} = supervisor:start_child(
                       list_to_atom(atom_to_list(Name)++"_sup"),
                       [Cmd]),

               {reply, Pid, State#state{limit=NewLimit} };
 

handle_call({start_worker, _}, _From, State) ->
    {reply, full_limit, State};


handle_call({stop_all_workers}, _From, #state{workers_pids=Pids}=State) ->
    ?Debug(Pids),
        lists:foreach(fun(Pid) -> gen_server:call(Pid, stop) end, 
                                   Pids),
	        {reply, ok, State};


handle_call({register, Pid}, _From, #state{workers_pids=Pids}=State) ->
    erlang:monitor(process, Pid),

	    {reply, ok, State#state{workers_pids=[Pid|Pids] } };


handle_call({call_worker, Msg}, _From, #state{name=Name}=State) ->
    
    Free=ets:select(Name, 
                   ets:fun2ms(fun(N=#worker_stat{status=P}) 
                                    when P=/=2 -> N 
                              end)
                  ),

      case Free of
          [] -> 
              {reply, {ok, []}, State};
          [P|_] -> 
            R=gen_server:call(P#worker_stat.pid, Msg),
            {reply, {ok, R}, State}

      end;



handle_call({call_workers, Msg}, _From, #state{name=Name}=State) ->
    
    Free=ets:select(Name, 
                   ets:fun2ms(fun(N=#worker_stat{status=P}) 
                                    when P=/=2 -> N 
                              end)
                  ),

        R=lists:map(fun(Pid) -> 
                            ?Debug({call, Pid}),
                             gen_server:call(Pid, Msg)
                    end, [X#worker_stat.pid||X<-Free]),
          ?Debug(R),
          
	        {reply, {ok, R}, State};


handle_call({get_result_worker, Msg}, _From, #state{name=Name}=State) ->
        

    %% TODO need new res ets
    R = ets:select(Name, 
                   ets:fun2ms(fun(N=#worker_stat{ref=P}) 
                                    when P=:=Msg -> N 
                              end)
                  ),

	        {reply, {ok, R}, State};


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast({cast_all_workers, Msg},  #state{workers_pids=Pids}=State) ->
    ?Debug(Pids),
        lists:foreach(fun(Pid) -> gen_server:cast(Pid, Msg) end,
                                   Pids),

	        {noreply, State};


handle_cast(_Msg, State) ->
	{noreply, State}.




handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, 
            #state{workers_pids=Pids, limit=Limit}=State) ->

    	{noreply, State#state{workers_pids=lists:delete(Pid, Pids),
                              limit=Limit+1}};



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


