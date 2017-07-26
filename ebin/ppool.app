{application, 'ppool', [
	{description, "Simple OPT ppool"},
	{vsn, "0.1.0"},
	{modules, ['ppool_app','ppool_sup']},
	{registered, [ppool_sup]},
	{applications, [kernel,stdlib]},
	{mod, {ppool_app, []}},
	{env, []}
]}.