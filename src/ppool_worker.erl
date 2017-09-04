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
         set_status_worker/3,
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
          workers_pids=maps:new()
          
}).


%% init

start_link(Name, Limit, MFA) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name, Limit, MFA], []).


init([Name, Limit, MFA]) ->
    Name = ets:new(Name, [set, public, named_table, 
                          {keypos, #worker_stat.ref}]),
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
	


call_workers(Name, Msg) ->
    gen_server:call(Name, {call_workers, {msg, Msg}}).

call_sync_workers(Name, Msg) ->
   gen_server:call(Name, {call_workers, {sync_msg, Msg}}).

%% call_workers(Name, Msg, Acc) ->
%%  case call_worker(Name, Msg) of
%%    {ok, []} -> {ok, Acc};
%%    {ok, R} -> call_workers(Name, Msg, [R|Acc])
%% 
%% end.


cast_all_workers(Name, Msg) ->
    gen_server:cast(Name, {cast_all_workers, {msg, Msg}}).


set_status_worker(Name, Pid, S) ->
    gen_server:cast(Name, {set_status_worker, Pid, S}).


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
    ?Debug(maps:keys(Pids)),
        lists:foreach(fun(Pid) -> gen_server:call(Pid, stop) end, 
                                   maps:keys(Pids)),
	        {reply, ok, State};


handle_call({register, Pid}, _From, #state{workers_pids=Pids}=State) ->
    erlang:monitor(process, Pid),

	    {reply, ok, State#state{workers_pids=maps:put(Pid, 0, Pids) } };


handle_call({call_worker, Msg}, _From, #state{workers_pids=Pids}=State) ->
    
    Free=maps:filter(fun(_K, V) -> V=/=2 end ,Pids),

    ?Debug(Free),

    case maps:keys(Free) of
          [] -> 
              {reply, {ok, []}, State};

          [P|_] -> 
            R=gen_server:call(P, Msg),

              {reply, {ok, R}, State}

      end;


handle_call({call_workers, Msg}, _From, #state{workers_pids=Pids}=State) ->
    
    Free=maps:filter(fun(_K, V) -> V=/=2 end ,Pids),

    ?Debug(Free),

        R=lists:map(fun(P) ->
                        gen_server:call(P, Msg)
                end
                ,maps:keys(Free)),

            {reply, {ok, R}, State};



handle_call({get_result_worker, Msg}, _From, #state{name=Name}=State) ->
        
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
                                   maps:keys(Pids)),

	        {noreply, State};

handle_cast({set_status_worker, Pid, S},
            #state{workers_pids=Pids}=State) ->

	{noreply, State#state{workers_pids=maps:update(Pid, S, Pids)}};



handle_cast(_Msg, State) ->
	{noreply, State}.




handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, 
            #state{workers_pids=Pids, limit=Limit}=State) ->

    	{noreply, State#state{workers_pids=maps:remove(Pid, Pids),
                              limit=Limit+1}};



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


