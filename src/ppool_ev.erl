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
          pid,
          filter,
          api
}).


start_link(Name) ->
	gen_event:start_link({local, list_to_atom(atom_to_list(Name)++"_ev")}).


init([Pid, Filter, API]) ->
	{ok, #state{pid=Pid, filter=Filter, api=API}}.




handle_event({msg, {F, _, Msg}=_M}, 
             #state{pid=Pid, filter=Filter, api=API}=State)
      when F=:=Filter,  API=:=all ->

     ?Debug({event_all, self(), Pid, Msg, Filter, F, API}),

         ppool_worker:cast_all_workers(Pid, Msg++"\n"),
    
      {ok, State};


handle_event({msg, {F, _, Msg}=_M}, 
             #state{pid=Pid, filter=Filter, api=API}=State)
      when F=:=Filter,  API=:=one ->

     ?Debug({event_one, self(), Pid, Msg, Filter, F, API}),


         case ppool_worker:call_worker(Pid, Msg++"\n") of
             {ok, []} -> 
                 error_logger:error_msg("no more subscribers ~p~n, [~p]",
                                                            [Pid, Msg]);
             {ok, _R} -> ok

         end,
    
      {ok, State};



handle_event(Event, State) ->
    ?Debug(Event),
      {ok, State}.
 

handle_call(_, State) ->
    {ok, ok, State}.
 

handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
