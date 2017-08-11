-module(worker).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("common.hrl").


-record(state, {
          restart_after_msg=10,
          master
}).

%% API.

-spec start_link(_) -> {ok, pid()}.
start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

%% gen_server.

init([Args]) ->
    ?Debug({Args, self()}),
	    {ok, #state{master=Args}, 0}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.



handle_cast({msg, T}, #state{restart_after_msg=C}=State) 
    when C > 0 ->
    
    timer:sleep(T),

    ?Debug({self(), C, ok}),
	    {noreply, State#state{restart_after_msg=C-1}};

handle_cast(_Msg, #state{restart_after_msg=C}=State) 
    when C=:=0 ->
        {stop, restart, State};

handle_cast(_Msg, State) ->
	{noreply, State}.




handle_info(timeout, #state{master=M}=State) ->
    ?Debug({registering, self()}),
     ppool_worker:register_worker(M, self()),
	  {noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.




terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

