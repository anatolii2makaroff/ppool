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

	{ok, _} = cowboy:start_clear(http, [{port, 8081}], #{
		env => #{dispatch => Dispatch}
        ,idle_timeout => 300000
	}),

	drop_api_sup:start_link().

stop(_State) ->
	ok.
