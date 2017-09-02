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

-include("ppool.hrl").


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



handle_call({msg, Msg}, From, #state{master=N, port=Port}=State) ->

    ?Debug(Msg),
    Ref = new_ets_msg(N, Msg),
      gen_server:reply(From, Ref),
 
       case process_ets_msg(N, Port, Ref, Msg) of
           {error, timeout} -> {stop, port_timeout, State};
            _ -> {noreply, State}
       end;

handle_call({sync_msg, Msg}, _From, #state{master=N, port=Port}=State) ->
 
    Ref = new_ets_msg(N, Msg),

        case process_ets_msg(N, Port, Ref, Msg) of
            {ok, Response} -> 
                {reply, {ok, Response}, State};
            {error, Status, Err} ->
                {reply, {error, Status, Err}, State};
            {error, timeout} ->
                 {stop, port_timeout, State}
        end;

handle_call(stop, From, State) ->
    ?Debug({stop, self()}),
    gen_server:reply(From, ok),
     {stop, normal, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.




handle_cast({msg, restart}, State) ->
    {stop, restart, State};

handle_cast({msg, stop}, State) ->
    {stop, normal, State};

handle_cast({msg, Msg}, #state{master=N, port=Port}=State) ->

    Ref = new_ets_msg(N, Msg),
 
       case process_ets_msg(N, Port, Ref, Msg) of
           {error, timeout} -> {stop, port_timeout, State};
            _ -> {noreply, State}

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

        true = ets:insert(M, 
                          #worker_stat{pid=self(), cmd=Cmd, status=0}),

	  {noreply, State#state{port=Port}};



handle_info({'EXIT', Port, Reason}, #state{port=Port}=State) ->
    ?Debug({exit, Reason}),
        {stop, {port_terminated, Reason}, State};

handle_info(_Info, State) ->
	{noreply, State}.



terminate({port_terminated, _Reason}, #state{master=M}=_State) ->
    true = ets:delete(M, self()),
    ok;

terminate(_Reason, #state{master=M, port=Port}=_State) ->
    true = ets:delete(M, self()),   
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
         600000 ->
            {error, timeout}
    end.



new_ets_msg(N, Msg) ->
    Ref = make_ref(),
    ?Debug({new_ets_msg, self()}),
     true=ets:update_element(N, self(), [{#worker_stat.ref,Ref},
                                 {#worker_stat.req, Msg},
                                 {#worker_stat.status, 2},
                                 {#worker_stat.time_start, os:timestamp()}
                                ]),
     Ref.


process_ets_msg(N, Port, Ref, Msg) ->

     port_command(Port, Msg),

        case collect_response(Port) of
            {ok, Response} -> 
                true=ets:update_element(N, self(), [{#worker_stat.ref,Ref},
                                 {#worker_stat.status, 1},
                                 {#worker_stat.time_end, os:timestamp()}
                                ]),

                {ok, Response};

            {error, Status, Err} ->

                {error, Status, Err};
            {error, timeout} ->

                 {error, timeout}
        end.



