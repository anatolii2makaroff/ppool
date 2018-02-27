
-ifdef(debug2).
    -define(Debug2(M), io:format("~p~n", [M])).
-else.
    -define(Debug2(M), void).
-endif.

-ifdef(debug3).
    -define(Debug3(M), io:format("~p~n", [M])).
-else.
    -define(Debug3(M), void).
-endif.



-define(NODE_API_WORKERS, 10).
-define(NODE_API_TIMEOUT, 10000).

-define(NODE_RRD_WORKERS, 1).
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

-define(FLOWER_SC_WORKERS, 1).
-define(FLOWER_SC_TIMEOUT, 20000).
-define(FLOWER_SC_VER, "0.1.0").
-define(FLOWER_SC_INTERVAL, 3).

-define(WEBBONE_WORKERS, 1).
-define(WEBBONE_TIMEOUT, 20000).
-define(WEBBONE_VER, "0.1.0").
