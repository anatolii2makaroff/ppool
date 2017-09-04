

-ifdef(debug).
    -define(Debug(M), io:format("~p~n", [M])).
-else.
    -define(Debug(M), void).
-endif.



-record(worker_stat, {
          ref,
          pid,
          cmd,
          req,
          status,
          result,
          time_start,
          time_end
}).


