
-ifdef(debug2).
    -define(Debug2(M), io:format("~p~n", [M])).
-else.
    -define(Debug2(M), void).
-endif.


-define(NODE_API_WORKERS, 2).
-define(NODE_API_TIMEOUT, 10000).

-define(NODE_RRD_WORKERS, 3).
-define(NODE_RRD_TIMEOUT, 10000).
-define(NODE_RRD_VER, "0.1.0").

-define(NODE_INFO_WORKERS, 1).
-define(NODE_INFO_INTERVAL, 5).
-define(NODE_INFO_TIMEOUT, 10000).

-define(NODE_CLTR_WORKERS, 1).
-define(NODE_CLTR_TIMEOUT, 10000).
-define(NODE_CLTR_VER, "0.1.0").

-define(NODE_INFO_IN_WORKERS, 1).
-define(NODE_INFO_IN_TIMEOUT, 10000).
-define(NODE_INFO_IN_TICK, 5000).

-define(FLOWER_WORKERS, 1).
-define(FLOWER_TIMEOUT, 10000).
-define(FLOWER_VER, "0.1.0").

