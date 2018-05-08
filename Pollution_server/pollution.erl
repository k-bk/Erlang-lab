-module(pollution).
-export([
         addStation/3,
         addValue/5,
         removeValue/4,
         getOneValue/4,
         getStationMean/3,
         getDailyMean/3,
         getMinimumDistanceStations/1
        ]).
-record(value, {name,
                date,
                pollutionType,
                amount}).

addStation(Name, Coord = {X,Y}, #{coords:= Coords, names := Names} = Monitor) 
  when is_list(Name) and is_number(X) and is_number(Y) ->
    case maps:is_key(Name, Names) or maps:is_key(Coord, Coords) of
        false -> Monitor#{coords => Coords#{Coord => Name}, names => Names#{Name => Coord}};
        true -> {error, station_exists}
    end.

addValue({_, _} = Coord, Date, PollutionType, Amount, #{coords := Coords} = Monitor) -> 
    case maps:is_key(Coord, Coords) of
        true -> addValue(maps:get(Coord, Coords), Date, PollutionType, Amount, Monitor);
        false -> {error, no_such_station}
    end;
addValue(Name, Date, PollutionType, Amount, #{names := Names, values := Values} = Monitor) -> 
    case maps:is_key(Name, Names) of
        true -> Monitor#{values => [#value{name = Name,
                                           date = Date,
                                           pollutionType = PollutionType,
                                           amount = Amount} | Values]}; 
        false -> {error, no_such_station}
    end.

removeValue({_, _} = Coord, Date, PollutionType, #{coords := Coords} = Monitor) -> 
    removeValue(maps:get(Coord, Coords), Date, PollutionType, Monitor);
removeValue(Name, Date, PollutionType, #{values := Values} = Monitor) -> 
    Monitor#{values => 
             lists:filter(fun(X) -> not valueFilter(X, Name, Date, PollutionType) end, Values)}.

getOneValue({_, _} = Coord, Date, PollutionType, #{coords := Coords} = Monitor) -> 
    getOneValue(maps:get(Coord, Coords), Date, PollutionType, Monitor);
getOneValue(Name, Date, PollutionType, #{values := Values}) -> 
    case lists:filter(fun(X) -> valueFilter(X, Name, Date, PollutionType) end, Values) of
        [] -> [];
        [H|_] -> H
    end.

valueFilter(X, Name, Date, PollutionType) ->
    {VDate, _} = X#value.date,
    X#value.name == Name andalso VDate == Date andalso X#value.pollutionType == PollutionType.

getStationMean(Name, PollutionType, #{values := Values}) ->
    Filtered = lists:filter(
            fun(X) -> 
                X#value.name == Name andalso X#value.pollutionType == PollutionType 
            end
            , Values),
    case length(Filtered) of
        0 -> {error, no_matching_results};
        Len -> lists:foldl(fun(X, Acc) -> X#value.amount + Acc end, 0, Filtered) / Len
    end.

getDailyMean(Date, PollutionType, #{values := Values}) ->
    Filtered = lists:filter(
            fun(X) -> 
                X#value.date == Date andalso X#value.pollutionType == PollutionType 
            end
            , Values),
    case length(Filtered) of
        0 -> {error, no_matching_results};
        Len -> lists:foldl(fun(X, Acc) -> X#value.amount + Acc end, 0, Filtered) / Len
    end.

distanceStations(S1, S2, #{names := Names}) ->
    {Ax,Ay} = maps:get(S1, Names),
    {Bx,By} = maps:get(S2, Names),
    (Ax-Bx) * (Ax-Bx) + (Ay-By) * (Ay-By).

getMinimumDistanceStations(#{names := Names} = Monitor) ->
    Dist = [{K1,K2, distanceStations(K1, K2, Monitor)} || K1 <- maps:keys(Names), K2 <- maps:keys(Names), K1 /= K2],
    case lists:keysort(3, Dist) of
        [] -> {error, no_matching_result};
        [H|_] -> H
    end.
