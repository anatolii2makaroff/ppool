

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
              ppool:start_pool(my, 10, {worker, start_link, []})
      end,
      fun(_) ->
              ppool:stop_pool(my)
      end,
      basic_tests()

     }
    }.


basic_tests() ->
    [
     {"worker run ok",
     fun() ->
             P1 = ppool_worker:run(my, 0),
             P2 = ppool_worker:run(my, 0),
             
             ?assert(P1=:=P2),
             ?assert(P1=:=ok)
 
     end
     },
     {"worker limit ",
     fun() ->
            
             P1 = for(10, fun ppool_worker:run/2, {my, 0}),
             % ?debugFmt("P1 = ~p~n", [P1]),
             ?assert(P1 =:= {error, full_limit})
 
     end
     }





    ].


for(N, Fun, {Pn, T}=Args) 
  when N>0 ->
    Fun(Pn, T),
    for(N-1, Fun, Args);

for(0, Fun, {Pn, T}) ->
    Fun(Pn, T).






