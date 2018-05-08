-module(pollution_server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, varSupervisor}, ?MODULE, []).

init(_) ->
    {ok, {
       {one_for_all, 2, 3},
       [ {pollution_server,
         {pollution_gen_server, start_link, []},
         permanent, 3, worker, [pollution_gen_server]}
       ]}
    }.
