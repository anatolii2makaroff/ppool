-module(ppool).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([start_pool/3, stop_pool/1]).

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

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, #state{}}.


start_pool(Name, Limit, MFA) ->
    gen_server:call(?MODULE, {start_pool, {Name, Limit, MFA}}).


stop_pool(Name) ->
    gen_server:call(?MODULE, {stop_pool, Name}).


handle_call({start_pool, {Name, Limit, MFA}}, _From, State) ->
    P = ppool_sup:start_pool(Name, Limit, MFA),

	{reply, P, State};


handle_call({stop_pool, Name}, _From, State) ->
    P = ppool_sup:stot_pool(Name),

	{reply, P, State};


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
