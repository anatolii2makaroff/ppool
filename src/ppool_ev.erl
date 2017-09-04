-module(ppool_ev).
-behaviour(gen_event).


%% API.
-export([start_link/1
        
        ]).

%% gen_event.
-export([init/1, 
         handle_event/2, 
         handle_call/2,
         handle_info/2, 
         code_change/3,
         terminate/2
        ]).
 

-include("ppool.hrl").

-record(state, {
          pid
}).


start_link(Name) ->
	gen_event:start_link({local, list_to_atom(atom_to_list(Name)++"_ev")}).


init([Pid]) ->
	{ok, #state{pid=Pid}}.



handle_event({msg, Msg}, #state{pid=Pid}=State) ->
    ?Debug({event, self(), Pid, Msg}),
      {ok, State};

handle_event(_Event, State) ->
      {ok, State}.
 


handle_call(_, State) ->
    {ok, ok, State}.
 


handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
