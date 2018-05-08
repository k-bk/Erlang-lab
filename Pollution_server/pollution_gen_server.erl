-module(pollution_gen_server).
-behaviour(gen_server).

%% API %%
-export([
         start_link/0,
         stop/0,
         crash/0,
         addStation/2,
         addValue/4,
         removeValue/3,
         getOneValue/3,
         getStationMean/2,
         getDailyMean/2,
         getMinimumDistanceStations/0,
         getState/0
        ]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2]).

%% START %%
start_link()   -> gen_server:start_link({local, pollution_server}, ?MODULE, [], []).

%% INTERFEJS KLIENT -> SERWER %%

addStation(Name, Coords) -> 
    gen_server:cast(pollution_server, {addStation, Name, Coords}).
addValue(Station, {Date, _Time}, Type, Amount) -> 
    gen_server:cast(pollution_server, {addValue, Station, Date, Type, Amount}).
removeValue(Station, {Date, _Time}, Type) -> 
    gen_server:cast(pollution_server, {removeValue, Station, Date, Type}).
getOneValue(Station, {Date, _Time}, Type) -> 
    gen_server:call(pollution_server, {getOneValue, Station, Date, Type}).
getStationMean(Station, Type) -> 
    gen_server:call(pollution_server, {getStationMean, Station, Type}).
getDailyMean({Date, _Time}, Type) -> 
    gen_server:call(pollution_server, {getDailyMean, Date, Type}).
getMinimumDistanceStations() ->
    gen_server:call(pollution_server, {getMinimumDistanceStations}).
getState() ->
    gen_server:call(pollution_server, {getState}).

stop()     -> gen_server:call(pollution_server, terminate).
init(_)     -> 
    Monitor = #{coords => #{}, names => #{},  values => []},
    {ok, Monitor}.

crash() -> 1/0.

%% OBSŁUGA WIADOMOŚCI %%
handle_cast({addStation, Name, Coords},	State) -> 
    Result = pollution:addStation(Name, Coords, State),
    case Result of 
        {error, _}  -> {noreply, State};
        _           -> {noreply, Result}
    end;
handle_cast({addValue, Station, Date, Type, Amount}, State) -> 
    Result = pollution:addValue(Station, Date, Type, Amount, State),
    case Result of 
        {error, _}  -> {noreply, State};
        _           -> {noreply, Result}
    end;
handle_cast({removeValue, Station, Date, Type}, State) -> 
    {noreply, pollution:removeValue(Station, Date, Type, State)}.

handle_call({getOneValue, Station, Date, Type}, _From, State) -> 
    {reply, pollution:getOneValue(Station, Date, Type, State), State};
handle_call({getStationMean, Station, Type}, _From, State) -> 
    {reply, pollution:getStationMean(Station, Type, State), State};
handle_call({getDailyMean, Date, Type}, _From, State) -> 
    {reply, pollution:getDailyMean(Date, Type, State), State};
handle_call({getMinimumDistanceStations}, _From, State) -> 
    {reply, pollution:getMinimumDistanceStations(State), State};
handle_call({getState},	_From, State) -> {reply, State, State};
handle_call(terminate,	_From, State) -> {stop, normal, ok, State}.

terminate(normal, State) -> io:format("The state is: ~B~nBye.~n",[State]), ok.
