-module(worker).
-behaviour(gen_server).

%% API.
-export([start_link/1, sleep/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {

}).

%% API.

-spec start_link(_) -> {ok, pid()}.
start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

%% gen_server.

init([_Args]) ->
	{ok, #state{}}.


handle_call({run, Fun, Args}, _From, State) ->

    Res = ?MODULE:Fun(Args),
	    {reply, Res, State};


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


sleep(T) ->
    timer:sleep(T),
      {ok, T}.



