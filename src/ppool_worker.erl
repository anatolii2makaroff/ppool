-module(ppool_worker).
-behaviour(gen_server).

%% API.
-export([start_link/3,
         checkin/1,
         run/3
        
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
          pids=
}).

%% API.

start_link(Name, Limit, MFA) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name, Limit, MFA], []).


checkin(Name) ->
    gen_server:call(Name, {checkin}).


run(Name, Task, Args) ->
    case checkin(Name) of 

       full_limit -> {error, full_limit};
        Pid -> gen_server:call(Pid, {run, Task, Args})

    end.

%% gen_server.


init([Name, Limit, MFA]) ->
	{ok, #state{limit=Limit, mfa=MFA, name=Name}}.



handle_call({checkin}, _From, #state{name=Name, limit=Limit, mfa=MFA}=State) 
  when Limit > 0 ->

    NewLimit = Limit - 1,
    {_, _, A} = MFA,

         {ok, Pid} = supervisor:start_child(
                       list_to_atom(atom_to_list(Name)++"_sup"),
                       [A]),

	      {reply, Pid, State#state{limit=NewLimit}};

handle_call({checkin}, _From, State) ->
    {reply, full_limit, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
