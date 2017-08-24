{application, 'ppool', [
	{description, "Simple OPT ppool"},
	{vsn, "0.1.0"},
	{modules, ['main','port_worker','ppool','ppool_app','ppool_master_worker_sup','ppool_sup','ppool_worker','ppool_worker_state','ppool_worker_sup','worker']},
	{registered, [ppool_sup]},
	{applications, [kernel,stdlib]},
	{mod, {ppool_app, []}},
	{env, []}
]}.