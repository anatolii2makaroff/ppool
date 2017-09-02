

-ifdef(debug).
    -define(Debug(M), io:format("~p~n", [M])).
-else.
    -define(Debug(M), void).
-endif.



-record(worker_stat, {
          pid,
          cmd,
          ref,
          req,
          status,
          time_start,
          time_end
}).


