%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2025 15:19
%%%-------------------------------------------------------------------
-module(pollution_calc).
-author("agada").

%% API
-export([sample_data/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

number_of_readings([], _Date) ->
  0;
number_of_readings([{_Station, {D, _Time}, _Measurement} | Tail], Date) ->
  case D =:= Date of
    true -> 1 + number_of_readings(Tail, Date);
    false -> number_of_readings(Tail, Date)
  end.

calculate_max(Readings, Type) ->
  Values = get_type_values(Readings, Type),
  case Values of
    [] -> 0;
    _  -> lists:max(Values)
  end.

calculate_mean(Readings, Type) ->
  Values = get_type_values(Readings, Type),
  case Values of
    [] -> 0;
    _  -> lists:sum(Values) / length(Values)
  end.

get_type_values([], _Type) ->
  [];
get_type_values([{_Station, _Date, {ThisType, Val}} | Tail], Type) ->
  case ThisType =:= Type of
    true  -> [Val | get_type_values(Tail, Type)];
    false -> get_type_values(Tail, Type)
  end.


sample_data() ->
  [
    {
      "InstytutInformatyki",
      {"2025-03-17", "14:00"},
      {pm10, 45.0}
    },
    {
      "InstytutInformatyki",
      {"2025-03-17", "14:00"},
      {pm25, 25.0}
    },
    {
      "Nisko",
      {"2025-03-17", "14:00"},
      {pm10, 60.0}
    },
    {
      "Nisko",
      {"2025-03-18", "10:00"},
      {temperature, 14.5}
    },
    {
      "STW",
      {"2025-03-18", "09:00"},
      {pm10, 70.0}
    },
    {
      "STW",
      {"2025-03-18", "09:00"},
      {humidity, 40.0}
    }
  ].