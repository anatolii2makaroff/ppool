-module(ppool_worker).
-behaviour(gen_server).

-include("common.hrl").

%% API.
-export([start_link/3,
         start_worker/1,
         start_all_workers/2,
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
          workers_pids=[]
          
}).

%% API.

start_link(Name, Limit, MFA) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name, Limit, MFA], []).


start_worker(Name) ->
    gen_server:call(Name, {start_worker}).


register_worker(Name, Pid) ->
    gen_server:call(Name, {register, Pid}).


start_all_workers(Name, Args) ->
    case start_worker(Name) of 

       full_limit -> {ok, full_limit};
        Pid -> gen_server:call(Pid, {run, Args}),
                start_all_workers(Name, Args)

    end.


call_all_workers(Name, Msg) ->
    gen_server:call(Name, {call_all_workers, Msg}).



init([Name, Limit, MFA]) ->
	{ok, #state{limit=Limit, mfa=MFA, name=Name}}.


handle_call({call_all_workers, Msg}, _From, #state{workers_pids=Pids}=State) ->

            ?Debug(Pids),
            Fun = call_worker_by_pid(Msg),
                lists:foreach(Fun, Pids),

	        {reply, ok, State};


handle_call({start_worker}, _From, #state{name=Name, 
                                          limit=Limit
                                         }=State ) 
  when Limit > 0 ->

    NewLimit = Limit - 1,

         {ok, Pid} = supervisor:start_child(
                       list_to_atom(atom_to_list(Name)++"_sup"),
                       []),

	        {reply, Pid, State#state{limit=NewLimit} };


handle_call({start_worker}, _From, State) ->
    {reply, full_limit, State};


handle_call({register, Pid}, _From, #state{workers_pids=Pids}=State) ->

    erlang:monitor(process, Pid),

	    {reply, ok, State#state{workers_pids=[Pid|Pids]} };


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, 
            #state{workers_pids=Pids}=State) ->

    NewPids = lists:delete(Pid, Pids),
    
    	{noreply, State#state{workers_pids=NewPids}};



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


call_worker_by_pid(Msg) ->
    fun(Pid) -> gen_server:cast(Pid, {msg, Msg}) end.


