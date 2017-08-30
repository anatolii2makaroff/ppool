-module(ppool_worker).
-behaviour(gen_server).

-include("common.hrl").

%% API.
-export([start_link/3,
         start_worker/2,
         start_all_workers/2,
         start_map_workers/2,
         stop_all_workers/1,
         cast_all_workers/2,
         call_all_workers/2,
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
          workers_map=maps:new()
          
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

call_all_workers(Name, Msg) ->
    gen_server:call(Name, {call_all_workers, Msg}).



init([Name, Limit, MFA]) ->
	{ok, #state{limit=Limit, mfa=MFA, name=Name}}.


handle_call({cast_all_workers, Msg}, _From, #state{workers_map=Pids}=State) ->
    ?Debug(maps:keys(Pids)),
        lists:foreach(fun(Pid) -> gen_server:cast(Pid, {msg, Msg}) end,
                                   maps:keys(Pids)),

	        {reply, ok, State};


handle_call({stop_all_workers}, _From, #state{workers_map=Pids}=State) ->
    ?Debug(maps:keys(Pids)),
        lists:foreach(fun(Pid) -> gen_server:call(Pid, stop) end, 
                                  maps:keys(Pids) ),

	        {reply, ok, State};



handle_call({call_all_workers, Msg}, _From, #state{workers_map=Pids}=State) ->
    
        R=lists:map(fun(Pid) -> 
                            ?Debug({call, Pid}),
                             gen_server:call(Pid, {msg, Msg})
                    end, maps:keys(Pids)),
          ?Debug(R),
          
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


handle_call({register, Pid}, _From, #state{workers_map=Pids}=State) ->

    erlang:monitor(process, Pid),

	    {reply, ok, State#state{workers_map=maps:put(Pid, 0, Pids) } };


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, 
            #state{workers_map=Pids, limit=Limit}=State) ->
    
    	{noreply, State#state{workers_map=maps:remove(Pid, Pids),
                              limit=Limit+1}};



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


