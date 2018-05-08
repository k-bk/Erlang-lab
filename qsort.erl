-module (qsort).
-compile (export_all).

lessThan(List, Arg) -> [ X || X<-List, X < Arg].
grtEqThan(List, Arg) -> [ X || X<-List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).

randomElems(N,Min,Max) ->
    [ rand:uniform(Max - Min + 1) + Min - 1 || _<-lists:seq(1, N) ].

compareSpeeds(List, Fun1, Fun2) -> 
    {Time1, _} = timer:tc(Fun1, [List]),
    {Time2, _} = timer:tc(Fun2, [List]),
    io:format("~w ~w ~n", [Time1, Time2]).

