%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pollution).

%% API
-export([
  create_monitor/0,
  add_station/3,
  add_value/5,
  remove_value/4,
  get_one_value/4,
  get_station_mean/3,
  get_daily_mean/3,
  get_daily_over_limit/3
]).

create_monitor() ->
  {[], []}.

add_station(Name, Coordinates, {Stations, Measurements}) ->
  case lists:any(fun({station, N, Coord}) ->
    N =:= Name orelse Coord =:= Coordinates
                 end, Stations) of
    true ->
      {error, "Station already exists"};
    false ->
      { [{station, Name, Coordinates} | Stations], Measurements }
  end.

add_value(StationId, DateTime, Type, Value, {Stations, Measurements}) ->
  case find_station(StationId, Stations) of
    {error, _} = Error ->
      Error;
    {ok, {station, StationName, Coord}} ->
      case lists:any(fun({measurement, _StationName, MCoord, MDateTime, MType, _Val}) ->
        (MCoord =:= Coord) andalso (MDateTime == DateTime) andalso (MType =:= Type)
                     end, Measurements) of
        true ->
          {error, "Measurement already exists"};
        false ->
          NewMeasurement = {measurement, StationName, Coord, DateTime, Type, Value},
          {Stations, Measurements ++ [NewMeasurement]}
      end
  end.

remove_value(StationId, DateTime, Type, {Stations, Measurements}) ->
  case find_station(StationId, Stations) of
    {error, _} = Error ->
      Error;
    {ok, {station, _StationName, Coord}} ->
      case remove_measurement(Measurements, Coord, DateTime, Type) of
        {error, _} = Err -> Err;
        {ok, NewMeasurements} ->
          {Stations, NewMeasurements}
      end
  end.

get_one_value(StationId, DateTime, Type, {Stations, Measurements}) ->
  case find_station(StationId, Stations) of
    {error, _} = Error ->
      Error;
    {ok, {station, _, Coord}} ->
      case lists:filter(fun({measurement, _StationName, MCoord, MDateTime, MType, _V}) ->
        (MCoord =:= Coord) andalso (MDateTime == DateTime) andalso (MType =:= Type)
                        end, Measurements) of
        [{measurement, _, _, _, _, Value} | _] ->
          Value;
        [] ->
          {error, "Measurement not found"}
      end
  end.

get_station_mean(StationId, Type, {Stations, Measurements}) ->
  case find_station(StationId, Stations) of
    {error, _} = Error ->
      Error;
    {ok, {station, _, Coord}} ->
      Values = [Value || {measurement, _, MCoord, _DateTime, MType, Value} <- Measurements,
        MCoord =:= Coord, MType =:= Type],
      case Values of
        [] ->
          {error, "No measurements for station and type"};
        _ ->
          lists:sum(Values) / length(Values)
      end
  end.

get_daily_mean(Type, Day, {_Stations, Measurements}) ->
  Filtered = [Value ||
    {measurement, _, _Coord, {{Y1, M1, D1}, _Time}, T, Value} <- Measurements,
    T =:= Type,
    {Y1, M1, D1} =:= Day],
  case Filtered of
    [] ->
      {error, "No measurements for given day and type"};
    _ ->
      lists:sum(Filtered) / length(Filtered)
  end.

get_daily_over_limit(Type, Day, {_Stations, Measurements}) ->
  case get_norm(Type) of
    undefined ->
      {error, "Norm not defined for parameter"};
    Norm ->
      StationCoords = [ Coord ||
        {measurement, _, Coord, {{Y1, M1, D1}, _Time}, T, Value} <- Measurements,
        T =:= Type,
        {Y1, M1, D1} =:= Day,
        Value > Norm
      ],
      UniqueStations = lists:usort(StationCoords),
      length(UniqueStations)
  end.

%%--------------------------------------------------------------------
%% POMOCNICZE
%%--------------------------------------------------------------------

find_station(Id, Stations) ->
  Filtered = lists:filter(fun({station, Name, Coord}) ->
    (is_tuple(Id) andalso Coord =:= Id) orelse
      (is_list(Id) andalso Name =:= Id)
                          end, Stations),
  case Filtered of
    [Station | _] -> {ok, Station};
    [] -> {error, "Station not found"}
  end.

remove_measurement([], _Coord, _DateTime, _Type) ->
  {error, "Measurement not found"};
remove_measurement([{measurement, _Name, Coord, DateTime, Type, _Value} | T], Coord, DateTime, Type) ->
  {ok, T};
remove_measurement([Head | T], Coord, DateTime, Type) ->
  case remove_measurement(T, Coord, DateTime, Type) of
    {ok, NewT} -> {ok, [Head | NewT]};
    Error -> Error
  end.

get_norm("PM10") -> 50;
get_norm("PM2,5") -> 25;
get_norm("PM1") -> 35;
get_norm(_) -> undefined.
