-module(ppool_worker).
-behaviour(gen_server).

-include("ppool.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API.
-export([start_link/3,
         start_worker/2,
         start_all_workers/2,
         start_map_workers/2,
         stop_all_workers/1,
         cast_all_workers/2,
         call_sync_all_workers/2,
         call_all_workers/2,
         get_result_worker/2,
         register_worker/2
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          limit=10, 
          mfa, 
          name,
          workers_pids=[]
          
}).



%% API.

start_link(Name, Limit, MFA) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name, Limit, MFA], []).


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


register_worker(Name, Pid) ->
    gen_server:call(Name, {register, Pid}).



cast_all_workers(Name, Msg) ->
    gen_server:call(Name, {cast_all_workers, Msg}).

call_sync_all_workers(Name, Msg) ->
    gen_server:call(Name, {call_sync_all_workers, Msg}).

call_all_workers(Name, Msg) ->
    gen_server:call(Name, {call_all_workers, Msg}).

get_result_worker(Name, Msg) ->
    gen_server:call(Name, {get_result_worker, Msg}).



init([Name, Limit, MFA]) ->
    Name = ets:new(Name, [bag, public, named_table, 
                          {keypos, #worker_stat.pid}]),
	{ok, #state{limit=Limit, mfa=MFA, name=Name}}.


handle_call({cast_all_workers, Msg}, _From, #state{workers_pids=Pids}=State) ->
    ?Debug(Pids),
        lists:foreach(fun(Pid) -> gen_server:cast(Pid, {msg, Msg}) end,
                                   Pids),

	        {reply, ok, State};


handle_call({stop_all_workers}, _From, #state{workers_pids=Pids}=State) ->
    ?Debug(Pids),
        lists:foreach(fun(Pid) -> gen_server:call(Pid, stop) end, 
                                  Pids),

	        {reply, ok, State};



handle_call({call_sync_all_workers, Msg}, _From, #state{name=Name}=State) ->
    
    Free=ets:select(Name, 
                   ets:fun2ms(fun(N=#worker_stat{status=P, pid=P}) 
                                    when P=:=1 orelse P=:=0 -> N 
                              end)
                  ),

        R=lists:map(fun(Pid) -> 
                            ?Debug({call, Pid}),
                             gen_server:call(Pid, {sync_msg, Msg})
                    end, [X#worker_stat.pid||X<-Free]),
          ?Debug(R),
          
	        {reply, {ok, R}, State};


handle_call({call_all_workers, Msg}, _From, #state{name=Name}=State) ->
    
    Free=ets:select(Name, 
                   ets:fun2ms(fun(N=#worker_stat{status=P}) 
                                    when P=:=1 orelse P=:=0 -> N 
                              end)
                  ),

        R=lists:map(fun(Pid) -> 
                            ?Debug({call, Pid}),
                             gen_server:call(Pid, {msg, Msg})
                    end, [X#worker_stat.pid||X<-Free]),
          ?Debug(R),
          
	        {reply, {ok, R}, State};


handle_call({get_result_worker, Msg}, _From, #state{name=Name}=State) ->
    
    R = ets:select(Name, 
                   ets:fun2ms(fun(N=#worker_stat{ref=P}) 
                                    when P=:=Msg -> N 
                              end)
                  ),

	        {reply, {ok, R}, State};



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


handle_call({register, Pid}, _From, #state{workers_pids=Pids}=State) ->

    erlang:monitor(process, Pid),

	    {reply, ok, State#state{workers_pids=[Pid|Pids] } };


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


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


