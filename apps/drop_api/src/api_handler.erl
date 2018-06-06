-module(api_handler).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

-include("api_handler.hrl").
-include("../../src/ppool.hrl").


init(_, Req, _) ->

	{Method, Req2} = cowboy_req:method(Req),
	 HasBody = cowboy_req:has_body(Req2),

     case create_req(Method, HasBody, Req2) of
	
	   {ok, Req3, Is_Gzip} ->
             {loop, Req3, Is_Gzip};

       {ok, Req3} ->
             {loop, Req3}
     end.


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

    {ok, Body, Req2} = cowboy_req:body(Req, [{length, infinity}]),

    %% ?Debug2({post_req, erlang:binary_to_atom(Flow, latin1), Body}),

    {L, _} = cowboy_req:body_length(Req),

    ?Debug2(L),

    case L of

        L when L<?MAX_BODY_REDIRECT -> 
            Pid = pg2:get_closest_pid(erlang:binary_to_atom(Flow, latin1)),
            %% Body2 = Body;
            Is_Gzip = false,

            Body2 = Body;

        L when L>?MAX_BODY_REDIRECT ->
            Pid = pg2:get_closest_pid(
                    erlang:binary_to_atom(
                      erlang:iolist_to_binary([Flow, <<"_X">>]), latin1)
                   ),

            Is_Gzip = true,
            Body2 = base64:encode(zlib:gzip(Body))

    end,

    ?Debug2({post_req_pid, Pid}),

    case is_pid(Pid) of

        false ->
            self()!{response, {error, mis_req_pool}};

        true ->
            case ppool_worker:cast_worker_defer(Pid, body_to_msg(Body2)) of
                {ok, ok} ->
                    ok;
                 _Err ->
                    self()!{response, {error, mis_run_pool}}

             end
            
    end,

     {ok, Req2, Is_Gzip}.


info({response, Res}, Req, Is_Gzip) ->
    %% {response,{ok,[<<"ping">>]}}
    %%

    ?Debug2({recv_post_req, Res}),

    case Res of
        {ok,[Msg]} ->

         case Is_Gzip of 
             true ->
                  Msg2 = uncompress(Msg); %% zlib:gunzip(Msg);
             _ ->
                 Msg2 = Msg
         end,

            cowboy_req:reply(200, [
		                {<<"content-type">>, <<"text/plain; charset=utf-8">>}
    	                ], msg_to_body(Msg2), Req);

        {error, mis_req_pool} ->
            cowboy_req:reply(400, [], <<"Missing Registered Pool">>, Req);

        {error, mis_run_pool} ->
            cowboy_req:reply(400, [], <<"Missing Running Pool">>, Req);

        _Any ->
            cowboy_req:reply(503, [], <<"Error occured">>, Req)
    end,

	{ok, Req, Is_Gzip};


info(Any, Req, State) ->

   error_logger:warning_msg("call no api ~p~n ", [Any]),
 
 	{ok, Req, State}.





terminate({normal, timeout}, _, _) ->
	ok;

terminate(_Reason, _Req, _State) ->
	ok.



msg_to_body(Body) ->
    case binary:last(Body) =:= 10 of
        true ->

            binary:replace(Body, ?SPLIT_MSG_SEQ, <<"\n">>, [global]);

        false ->
            Body2 = binary:replace(Body, ?SPLIT_MSG_SEQ, <<"\n">>, [global]),
           <<Body2/binary, <<"\n">>/binary>>
    end.



body_to_msg(B) ->

   Body = binary:replace(B, <<"\r\n">>, <<"\n">>, [global]),
 
    case binary:last(Body) =:= 10 of
        true ->
            Body2 = binary:part(Body, {0, byte_size(Body)-1}),
             Body3 = binary:replace(Body2, <<"\n">>, ?SPLIT_MSG_SEQ, [global]),
              ?Debug3({Body, Body2, Body3}),

              <<Body3/binary, <<"\n">>/binary>>;

        false ->
            Body2 = binary:replace(Body, <<"\n">>, ?SPLIT_MSG_SEQ, [global]),
              ?Debug3({Body, Body2}),


             <<Body2/binary, <<"\n">>/binary>>
    end.


uncompress(Bin) ->
   Z = zlib:open(),
   zlib:inflateInit(Z, 31),
   
   % hopefully everything fits in memory
   
   Uncompressed = zlib:inflate(Z, 
                               base64:decode(
                                 binary:replace(Bin, ?SPLIT_MSG_SEQ, <<"\n">>, [global])
                                )
                              ),
   
   zlib:inflateEnd(Z),
   zlib:close(Z),

     erlang:list_to_binary(Uncompressed).




