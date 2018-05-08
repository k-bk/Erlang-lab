-module(pollution_gen_server).
-behaviour(gen_server).

%% API %%
-export([start_link/0, increment/1,get/1,close/1]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2]).

%% START %%
start_link()   -> gen_server:start_link(?MODULE,0,[]).

%% INTERFEJS KLIENT -> SERWER %%
addStation(Pid, Name, Coords) -> 
    gen_server:cast(Pid,{addStation, Name, Coords})).
addValue(Station, {Date, _Time}, Type, Amount) -> 
    gen_server:cast(pollution_server, {addStation, Station, Date, Type, Amount}).
close(Pid)     -> gen_server:call(Pid,terminate).
init(N)        -> 
    Monitor = #{coords => #{}, names => #{},  values => []},
    {ok, Monitor}.

%% OBSŁUGA WIADOMOŚCI %%
handle_cast({addStation, Name, Coords},	_From, Monitor) -> 
    {noreply, addStation(Name, Coords, Monitor)}.
handle_cast({addValue, Station, Date, Type, Amount}, _From, Monitor) -> 
    {noreply, addValue(Station, Date, Type, Amount, Monitor)}.
handle_cast({removeValue, Station, Date, Type}, _From, Monitor) -> 
    {noreply, addValue(Station, Date, Type, Monitor)}.

handle_call(getMonitor,	_From, Monitor) -> {reply, Monitor, Monitor};
handle_call(getMonitor,	_From, Monitor) -> {reply, Monitor, Monitor};
handle_call(terminate,	_From, Monitor) -> {stop, normal, ok, Monitor}.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.
