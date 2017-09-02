

-module(ppool_tests).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

ppool_test_() ->
    {setup,
     fun() ->
             application:start(ppool)
     end,
     fun(_) ->
             application:stop(ppool)
     end,
     {
      foreach,
      fun() ->
              ppool:start_pool(test, 10, {port_worker, start_link, []})
      end,
      fun(_) ->
              ppool:stop_pool(test)
      end,
      basic_tests()

     }
    }.


basic_tests() ->
    [
     {"start_worker",
     fun() ->
             P1 = ppool_worker:start_worker(test, "python ./priv/hello.py 1"
                                                  "2>> ./logs/hello_err.log"),
             P2 = ppool_worker:start_worker(test, "python ./priv/hello.py 2"
                                                  "2>> ./logs/hello_err.log"),
             P3 = ppool_worker:start_worker(test, "python ./priv/hello2.py 2"
                                                  "2>> ./logs/hello_err.log"),


             ?assert(P1=/=P2)
     end
     }

    ].


for(N, Fun, {Pn, T}=Args) 
  when N>0 ->
    Fun(Pn, T),
    for(N-1, Fun, Args);

for(0, Fun, {Pn, T}) ->
    Fun(Pn, T).






