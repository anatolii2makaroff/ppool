-module(api_handler).

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export([create_req/2, 
         create_res/2
        ]).

-include("api_handler.hrl").

%% Custom callbacks

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"octet-stream">>, []}, create_res}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"octet-stream">>, []}, create_req}],
		Req, State}.



%% handle GET
%%

create_res(Req, State) ->

	case cowboy_req:binding(flow, Req) of
		{undefined, Req2} ->

	      {<<"no flow\n">>, Req2, State};

		{Flow, Req2} ->
            ?Debug2({from_get, Flow}),

	         {<<"flow">>, Req2, State};

        _Any  ->
            ?Debug2({bad_req, _Any}),

	         {<<"bad req\n">>, Req, State}


	end.


%% handle POST
%%

create_req(Req, State) ->

	case cowboy_req:binding(flow, Req) of

		{undefined, Req2} ->
            {false, Req2, State};

		{Flow, Req2} ->

            {ok, Body, Req3} = cowboy_req:body(Req2),

            case pg2:get_closest_pid(erlang:binary_to_atom(Flow, latin1)) of
                {error, _} ->
                      ?Debug2(no_ppool_found),
                      _Response = <<"no_ppool_found">>;
                P ->
                      _Response = ppool_worker:call_worker(P, Body),
                      ?Debug2({from_post, P, Flow, Body, _Response})

              end,

            Req4 = cowboy_req:set_resp_body(Body, Req3),

			{true, Req4, State};

        _Any  ->
            ?Debug2({bad_req, _Any}),

	         {false, Req, State}


	end.





