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


