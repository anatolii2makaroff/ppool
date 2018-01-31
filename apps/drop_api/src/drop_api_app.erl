-module(drop_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
                      {'_', [
                          {"/api/v1/[:flow]", api_handler, []}
                      ]}
    ]),

    cowboy:start_http(http_listener, 100, [{port, 8081}],
                                    [{env, [{dispatch, Dispatch}]}]
                                        ),
	drop_api_sup:start_link().

stop(_State) ->
	ok.