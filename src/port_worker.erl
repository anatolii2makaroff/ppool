-module(port_worker).
-behaviour(gen_server).

%% API.
-export([start_link/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("common.hrl").


-record(state, {
          master,
          port,
          cmd
}).

%% API.

start_link(N, Cmd) ->
 
	gen_server:start_link(?MODULE, {N, Cmd}, []).

%% gen_server.

init({N, Cmd}) ->
    ?Debug({N, Cmd,  self()}),
      process_flag(trap_exit, true),

	    {ok, #state{master=N, cmd=Cmd}, 0}.



handle_call({msg, Msg}, _From, #state{port=Port}=State) ->
    
    port_command(Port, Msg),
        case collect_response(Port) of
            {ok, Response} -> 
                {reply, {ok, Response}, State};
            {error, Status, Err} ->
                {reply, {error, Status, Err}, State};
            {error, timeout} ->
                 {stop, port_timeout, State}
        end;


handle_call(stop, _From, State) ->
        {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.




handle_cast({msg, Msg}, #state{port=Port}=State) ->
    
    port_command(Port, Msg),
        case collect_response(Port) of
            {ok, _Response} -> 
                {noreply, State};
            {error, _Status, _Err} ->
                {noreply, State};
            {error, timeout} ->
                 {stop, port_timeout, State}
        end;


handle_cast(_Msg, State) ->
	{noreply, State}.



handle_info(timeout, #state{master=M, cmd=Cmd}=State) ->

    ?Debug({open_port, Cmd}),
 
      Port = open_port({spawn, Cmd},
                           [{line, 4096}, 
                              exit_status, binary]),
    
        ?Debug({registering, self()}),
        ppool_worker:register_worker(M, self()),
 
	  {noreply, State#state{port=Port}};



handle_info({'EXIT', Port, Reason}, #state{port=Port}=State) ->
    ?Debug({exit, Reason}),

        {stop, {port_terminated, Reason}, State};


handle_info(_Info, State) ->
	{noreply, State}.


terminate({port_terminated, _Reason}, _State) ->
    ok;

terminate(_Reason, #state{port=Port}=_State) ->
    port_close(Port);

terminate(_Reason, _State) ->
	ok.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.




collect_response(Port) ->
    collect_response(Port, [], <<>>).

collect_response(Port, Lines, OldLine) ->
   receive
        {Port, {data, Data}} ->
            case Data of
                {eol, Line} ->
                    {ok, [<<OldLine/binary,Line/binary>> | Lines]};
                {noeol, Line} ->
                    collect_response(Port, Lines, <<OldLine/binary,Line/binary>>)
            end;
        {Port, {exit_status, Status}} ->
            {error, Status, Lines}

    after
         30000 ->
            {error, timeout}
    end.

