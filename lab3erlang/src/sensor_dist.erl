%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. mar 2025 18:11
%%%-------------------------------------------------------------------
-module(sensor_dist).

-export([
  get_rand_locations/1,
  dist/2,
  find_for_person/2,
  find_closest/2,
  find_for_person/3,
  find_closest_parallel/2,
  run/0,
  run_parallel/0
]).

get_rand_locations(Number) ->
  [ {random:uniform(10000), random:uniform(10000)} || _ <- lists:seq(1, Number) ].

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

find_for_person(PersonLocation, SensorsLocations) ->
  Distances = [ {dist(PersonLocation, Sensor), {PersonLocation, Sensor}}
    || Sensor <- SensorsLocations ],
  lists:min(Distances).

find_closest(PeopleLocations, SensorsLocations) ->
  Results = [ find_for_person(Person, SensorsLocations)
    || Person <- PeopleLocations ],
  lists:min(Results).

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  Result = find_for_person(PersonLocation, SensorsLocations),
  ParentPID ! Result.

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  Parent_PID = self(),
  [ spawn(fun() ->
    find_for_person(Person, SensorsLocations, Parent_PID)
          end)
    || Person <- PeopleLocations ],

  Results = [ receive Res -> Res end || _ <- PeopleLocations ],
  lists:min(Results).

run() ->
  Sensors = get_rand_locations(1000),
  People  = get_rand_locations(20000),
  {Time, Result} = timer:tc(sensor_dist, find_closest, [People, Sensors]),
  io:format("Wynik sekwencyjny: ~p, czas: ~p mikrosekund~n", [Result, Time]),
  Result.

run_parallel() ->
  Sensors = get_rand_locations(1000),
  People  = get_rand_locations(20000),
  {Time, Result} = timer:tc(sensor_dist, find_closest_parallel, [People, Sensors]),
  io:format("Wynik równoległy: ~p, czas: ~p mikrosekund~n", [Result, Time]),
  Result.
