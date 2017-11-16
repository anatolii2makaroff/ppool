-module(node_watch).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

-define(TIMEOUT, 5000).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->

    net_kernel:monitor_nodes(true, [nodedown_reason]),

    erlang:send_after(?TIMEOUT, self(), ping_nodes),

	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info(ping_nodes, State) ->

    %% error_logger:info_msg("pinging nodes ~p~n",[net_adm:host_file()]),

    net_adm:world(),

    erlang:send_after(?TIMEOUT, self(), ping_nodes),

	{noreply, State};



handle_info({nodedown, Node, InfoList}, State)->

    error_logger:error_msg("node ~p is down: ~p~n",[Node, InfoList]),

    [{nodedown_reason,I}] = InfoList,

        Msg=erlang:list_to_bitstring(["system::node_watch::", 
                                       erlang:atom_to_list(Node), "::", 
                                       "nodedown::", 
                                       erlang:atom_to_list(I), "\n"]),

        node_scheduler:call(node(),
                            fun(N, C) -> 
                                    ppool_worker:call_worker(N, C)
                            end,
                            node_collector,
                            Msg
                           ),

	{noreply, State};


handle_info({nodeup, Node, _InfoList}, State) ->

    error_logger:warning_msg("node ~p is up~n",[Node]),


        Msg=erlang:list_to_bitstring(["system::node_watch::", 
                                       erlang:atom_to_list(Node), "::", 
                                       "nodeup::", 
                                       "nodeup..", "\n"]),

        node_scheduler:call(node(),
                            fun(N, C) -> 
                                    ppool_worker:call_worker(N, C)
                            end,
                            node_collector,
                            Msg
                           ),

    {noreply, State};


handle_info(Info, State) ->
    error_logger:info_msg("unknow msg ~p~n",[Info]),

	{noreply, State}.




terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
