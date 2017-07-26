-module(ppool_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link(Name, Limit, MFA) ->
	supervisor:start_link({local, Name}, ?MODULE, [Name, Limit, MFA]).

init(Name, Limit, MFA) ->
    Ppool_worker = {Name,
             {ppool_worker, start_link, [Limit]},
             permanent,
             brutal_kill,
             worker,
             [ppool_worker]
     },



	Procs = [Ppool_worker, ],
	{ok, {{one_for_one, 1, 5}, Procs}}.

