-module(pollution_client).
-compile(export_all).

addStation(Name, Coord) ->
    call({addStation, Name, Coord}).

addValue(Station, {Date,_Time}, PollutionType, Amount) ->
    call({addValue, Station, Date, PollutionType, Amount}).

removeValue(Station, {Date,_Time}, PollutionType) ->
    call({removeValue, Station, Date, PollutionType}).

getOneValue(Station, {Date,_Time}, PollutionType) ->
    call({getOneValue, Station, Date, PollutionType}).

getStationMean(Station, PollutionType) ->
    call({getStationMean, Station, PollutionType}).

getMinimumDistanceStations() ->
    call({getMinimumDistanceStations}).

stop() ->
    call(stop).

crash() ->
    pollution_server ! crash.

call({Args}) ->
    pollution_server ! {self(), Args},
    receive
        {_Pid, Val} -> io:format("~w~n",[Val]);
        Val -> io:format("~w~n",[Val])
    end.

