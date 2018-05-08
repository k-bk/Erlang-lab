-module(pingpong).
-compile(export_all).

start() ->
    PID1 = spawn(pingpong, ping_loop, []),
    PID2 = spawn(pingpong, pong_loop, []),
    register(ping, PID1),
    register(pong, PID2).

stop() ->
    ping ! stop,
    pong ! stop.

play(N) ->
    ping ! N.

ping_loop() ->
    receive
	stop -> terminate();
	0 -> terminate();
	N -> 
	    timer:sleep(500),
	    pong ! (N-1), 
	    io:format("Ping got ~w~n", [N]),
	    ping_loop()
    after
	20000 -> 
	    io:format("Terminating ~w~n", [self()]),
	    terminate()
    end.

pong_loop() ->
    receive
	stop -> terminate();
	0 -> terminate();
	N -> 
	    timer:sleep(500),
	    ping ! (N-1), 
	    io:format("Pong got ~w~n", [N]),
	    pong_loop()
    after
	20000 -> 
	    io:format("Terminating ~w~n", [self()]),
	    terminate()
    end.

terminate() ->
    ok.
    

