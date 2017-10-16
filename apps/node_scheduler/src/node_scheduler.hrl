
-ifdef(debug).
    -define(Debug(M), io:format("~p~n", [M])).
-else.
    -define(Debug(M), void).
-endif.


-define(NODE_API_WORKERS, 10).
-define(NODE_API_TIMEOUT, 10000).

-define(NODE_RRD_WORKERS, 3).
-define(NODE_RRD_TIMEOUT, 10000).
-define(NODE_RRD_VER, "0.1.0").

-define(NODE_INFO_WORKERS, 1).
-define(NODE_INFO_INTERVAL, 5).
-define(NODE_INFO_TIMEOUT, 10000).

-define(NODE_CLTR_WORKERS, 3).
-define(NODE_CLTR_TIMEOUT, 10000).
-define(NODE_CLTR_VER, "0.1.0").

