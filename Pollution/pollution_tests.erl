-module(pollution_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

createMonitor_test() -> 
	Mon = pollution:createMonitor(),
	?assertEqual(#{values => #{}, stations => #{}}, Mon).

addStation_test() ->
	Mon = pollution:createMonitor(),
	?assertEqual(Mon#{stations => #{"St1" => {5.5, 6.6}}}
	        , pollution:addStation("St1", {5.5, 6.6}, Mon)).



%addValue(Station, Date, PollutionType, Value, Monitor) -> Monitor.

%removeValue(Station, Date, PollutionType, Monitor) -> Monitor.

%getOneValue(PollutionType, Date, Station, Monitor) -> Monitor.

%getStationMean(PollutionType, Station, Monitor) -> Monitor.

%getDailyMean(PollutionType, Date, Monitor) -> Monitor.
