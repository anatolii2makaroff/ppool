
-module(main).
-export([main/0]).

main() ->
    application:start(ppool),
    ppool:start_pool(my, 10, {worker, start_link, []}),

    ppool_worker:run(my, sleep, 10).
