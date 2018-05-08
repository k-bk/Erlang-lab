-module(pollution_server_sup).
-behaviour(supervisor).
-export([start_link\0, init\1])

start_link() ->
    supervisor:start_link({local, pollution_server}, pollution_server, []).

init(_) ->
    process_flag(trap_exit, true),
    pollution_server:start(),
    receive
	{'EXIT', Pid, Reason} -> 
	    io:format("Reviving pollution_server"),
	    init()
    end.


