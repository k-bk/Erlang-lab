-module(calc).
-compile (export_all).

calc([], [S]) -> S;
calc(["+"|T], [B,A|S]) -> calc(T, [A+B|S]);
calc(["-"|T], [B,A|S]) -> calc(T, [A-B|S]);
calc(["*"|T], [B,A|S]) -> calc(T, [A*B|S]);
calc(["/"|T], [B,A|S]) -> calc(T, [A/B|S]);
calc(["^"|T], [B,A|S]) -> calc(T, [math:pow(B,A)|S]);
calc(["sqrt"|T], [A|S]) -> calc(T, [math:sqrt(A)|S]);
calc(["sin"|T], [A|S]) -> calc(T, [math:sin(A)|S]);
calc(["cos"|T], [A|S]) -> calc(T, [math:cos(A)|S]);
calc(["tan"|T], [A|S]) -> calc(T, [math:tan(A)|S]);
calc([X|T], S) -> 
    case string:to_float(X) of
	{error, no_float} -> calc(T, [list_to_integer(X)|S]);
	{V, _} -> calc(T, [V|S])
    end.

rpn(Args) ->
    calc(string:tokens(Args, " "), []).

calc2(fun F/2 end, [A|T]) -> [F(A)|T];
calc2(fun F/1 end, [A,B|T]) -> [F(B,A)|T].
