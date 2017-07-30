

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
              ppool_sup:start_pool(my, 10, {worker, start_link, []})
      end,
      fun(_) ->
              ppool_sup:stop_pool(my)
      end,
      basic_tests()

     }
    }.


basic_tests() ->
    [
     {"children limit",
     fun() ->
             P1 =ppool_worker:checkin(my),
             P2 =ppool_worker:checkin(my),
             ?assert(P1=/=P2)
     end
     },

     {"blah blah blah..",
     fun() ->
             
             P1 =ppool_worker:checkin(my),
             P2 =ppool_worker:checkin(my),
             ?assert(2=:=P2)
     end
     }



    ].
