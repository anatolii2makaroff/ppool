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

-record(state, {
          restart_after_msg=10
}).

%% API.

-spec start_link(_) -> {ok, pid()}.
start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

%% gen_server.

init([Args]) ->
    gen_server:call(Args, {register, self()}),
	    {ok, #state{restart_after_msg=Args}}.

handle_call({run, T}, _From, State) ->
    
    timer:sleep(T),
	    {reply, ok, State};



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

