

-ifdef(debug).
    -define(Debug(M), io:format("~p~n", [M])).
-else.
    -define(Debug(M), void).
-endif.


-ifdef(debug1).
    -define(Debug1(M), io:format("~p~n", [M])).
-else.
    -define(Debug1(M), void).
-endif.



-record(worker_stat, {
          ref,
          ref_from,
          pid,
          cmd,
          req,
          status,
          result,
          time_start,
          time_end
}).


