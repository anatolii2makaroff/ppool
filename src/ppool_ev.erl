-module(ppool_ev).
-behaviour(gen_event).


%% API.
-export([start_link/3,
        
        ]).

%% gen_event.
-export([init/1, 
         handle_event/2, 
         handle_call/2,
         handle_info/2, 
         code_change/3,
         terminate/2]).
 
-record(state, {
          master,
          mfa
}).


start_link(Name, MFA) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name, MFA], []).


init([Name, MFA]) ->
	{ok, #state{mfa=MFA, master=Name}}.


 
handle_event(_, State) ->
{ok, State}.
 
handle_call(_, State) ->
{ok, ok, State}.
 
handle_info(_, State) ->
{ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
{ok, State}.
 
terminate(_Reason, _State) ->
ok.
