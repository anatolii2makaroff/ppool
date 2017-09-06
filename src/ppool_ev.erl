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


%% all

handle_event({msg, {_,R,[Msg]}=_M}, 
             #state{pid=Pid, filter=Filter, api=API}=State)
      when Filter=/=no, API=:=all ->

    ?Debug({event_all, self(), Pid, Msg, Filter, API}),

     case binary:match(Msg, Filter) of
         nomatch -> ok;
               _ -> ppool_worker:cast_all_workers(Pid, R, [Msg]++"\n")
     end,
    
      {ok, State};


handle_event({msg, {_,R,[Msg]}=_M}, 
             #state{pid=Pid, filter=Filter, api=API}=State)
      when API=:=all ->

    ?Debug({event_all, self(), Pid, Msg, Filter, API}),
      ppool_worker:cast_all_workers(Pid, R, [Msg]++"\n"),
    
      {ok, State};


%% one

handle_event({msg, {_,R,[Msg]}=_M}, 
             #state{pid=Pid, filter=Filter, api=API}=State)
      when Filter=/=no, API=:=one ->

    ?Debug({event_one, self(), Pid, Msg, Filter, API}),

     case binary:match(Msg, Filter) of
         nomatch -> ok;
               _ -> call_worker(Pid, R, [Msg]++"\n")
     end,
 
      {ok, State};


handle_event({msg, {_,R,[Msg]}=_M}, 
             #state{pid=Pid, filter=Filter, api=API}=State)
      when API=:=one ->

     ?Debug({event_one, self(), Pid, Msg, Filter, API}),

         call_worker(Pid, R, [Msg]++"\n"),
    
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


call_worker(Pid, R, Msg) ->

         case ppool_worker:call_worker(Pid, R, Msg) of
             {ok, []} -> 
                 error_logger:error_msg("no more subscribers ~p~n, [~p]",
                                                            [{Pid,R}, Msg]);
             {ok, _Res} -> ok

         end.

