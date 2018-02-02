-module(api_handler).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-include("api_handler.hrl").



init(_, Req, State) ->

	{Method, Req2} = cowboy_req:method(Req),
	 HasBody = cowboy_req:has_body(Req2),
	
	  {ok, Req3} = create_req(Method, HasBody, Req2),

	{loop, Req3, State, 5000}.


create_req(<<"POST">>, true, Req) ->
	{Flow, Req2} = cowboy_req:binding(flow, Req),
        echo(Flow, Req2);

create_req(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

create_req(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).


echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing flow name">>, Req);

echo(Flow, Req) ->

    {ok, Body, Req2} = cowboy_req:body(Req),

    ?Debug2({post_req, erlang:binary_to_atom(Flow, latin1), Body}),

    Pid = pg2:get_closest_pid(erlang:binary_to_atom(Flow, latin1)),

    ?Debug2({post_req_pid, Pid}),

    case Pid of

        {error,{no_such_group,_}} ->
            cowboy_req:reply(400, [], <<"Missing Registered Pool">>, Req2);

        Pid ->
            case ppool_worker:cast_worker_defer(Pid, 
                                                <<Body/binary, <<"\n">>/binary>>) of
                {ok, ok} ->
                    ok;
                 _Err ->
                    cowboy_req:reply(400, [], 
                                     <<"Missing Running Pool">>, Req2)
            end
            
    end,

     {ok, Req2}.


info({response, Res}, Req, State) ->
    %% {response,{ok,[<<"ping">>]}}
    %%

    ?Debug2({recv_post_req, Res}),

    case Res of
        {ok,[Msg]} ->
            {ok, Req2} = cowboy_req:reply(200, [
		                {<<"content-type">>, <<"text/plain; charset=utf-8">>}
    	                ], Msg, Req);
        _Any ->
            {ok, Req2} = cowboy_req:reply(503, [], <<"Error occured">>, Req)
    end,

	{ok, Req2, State}.


terminate({normal, timeout}, _, _) ->
	ok;

terminate(_Reason, _Req, _State) ->
	ok.





