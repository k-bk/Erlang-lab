defmodule PollutionData do
  @moduledoc false
  def importLinesFromCSV (path \\ "pollution.csv") do
    tokens = File.read!(path) |>
      String.split("\r\n")
    tokens
  end

  def convertLine (line) do
    [date, time, len, wid, value] = String.split(line, ",")

    date_l = String.split(date, "-") |>
      Enum.reverse |>
      Enum.map(fn (x) -> elem(Integer.parse(x), 0) end) |>
      :erlang.list_to_tuple

    time_l = String.split(time <> ":00", ":") |>
      Enum.map(fn (x) -> elem(Integer.parse(x), 0) end) |>
      :erlang.list_to_tuple

    loc = {elem(Float.parse(len), 0), elem(Float.parse(wid), 0)}
    val = elem(Integer.parse(value), 0)
    %{:datetime => {date_l, time_l}, :location => loc, :pollutionLever => val}
  end

  def identifyStations (path \\ "../pollution.csv")  do
    stations = importLinesFromCSV(path) |>
      Enum.reduce(%{}, fn (x, acc) -> Map.put(acc, convertLine(x).location, "") end)
    Map.size(stations)
  end

  def station_name (%{logitude, latitude}) do
  end
end
