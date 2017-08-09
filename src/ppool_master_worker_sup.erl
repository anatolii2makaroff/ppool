-module(ppool_master_worker_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(Name, Limit, MFA) ->
	supervisor:start_link(?MODULE, [Name, Limit, MFA]).

init([Name, Limit, MFA]) ->
    Ppool_worker = {ppool_worker,
                    {ppool_worker, start_link, [Name, Limit, MFA]},
                    permanent,
                    brutal_kill,
                    worker,
                    [ppool_worker]
     },

    Ppool_worker_sup = {ppool_worker_sup,
                        {ppool_worker_sup, start_link, [Name, MFA]},
                        permanent,
                        3600,
                        supervisor,
                        [ppool_worker_sup] 
     },

	Procs = [Ppool_worker, Ppool_worker_sup],
	{ok, {{one_for_all, 1, 5}, Procs}}.

