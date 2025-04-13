%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. kwi 2025 13:20
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("agada").

%% API
-export([ start/0,
  stop/0,
  add_station/2,
  add_value/4,
  remove_value/3,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/2,
  get_daily_over_limit/2,
  init/0
]).

start() ->
  case whereis(pollution_server) of
    undefined ->
      Pid = spawn(fun init/0),
      register(pollution_server, Pid),
      {ok, Pid};
    _Pid ->
      {error, already_started}
  end.

stop() ->
  case whereis(pollution_server) of
    undefined -> {error, not_running};
    Pid ->
      Pid ! stop,
      unregister(pollution_server),
      ok
  end.

add_station(Name, Coordinates) ->
  call_server({add_station, Name, Coordinates}).

add_value(Station, DateTime, Type, Value) ->
  call_server({add_value, Station, DateTime, Type, Value}).

remove_value(Station, DateTime, Type) ->
  call_server({remove_value, Station, DateTime, Type}).

get_one_value(Station, DateTime, Type) ->
  call_server({get_one_value, Station, DateTime, Type}).

get_station_mean(Station, Type) ->
  call_server({get_station_mean, Station, Type}).

get_daily_mean(Type, Day) ->
  call_server({get_daily_mean, Type, Day}).

get_daily_over_limit(Type, Day) ->
  call_server({get_daily_over_limit, Type, Day}).


init() ->
  State = pollution:create_monitor(),
  loop(State).


loop(State) ->
  receive
    {call, From, {add_station, Name, Coordinates}} ->
      Result = pollution:add_station(Name, Coordinates, State),
      From ! {reply, Result},
      loop(Result);

    {call, From, {add_value, Station, DateTime, Type, Value}} ->
      Result = pollution:add_value(Station, DateTime, Type, Value, State),
      From ! {reply, Result},
      loop(Result);

    {call, From, {remove_value, Station, DateTime, Type}} ->
      Result = pollution:remove_value(Station, DateTime, Type, State),
      From ! {reply, Result},
      loop(Result);

    {call, From, {get_one_value, Station, DateTime, Type}} ->
      Reply = pollution:get_one_value(Station, DateTime, Type, State),
      From ! {reply, Reply},
      loop(State);

    {call, From, {get_station_mean, Station, Type}} ->
      Reply = pollution:get_station_mean(Station, Type, State),
      From ! {reply, Reply},
      loop(State);

    {call, From, {get_daily_mean, Type, Day}} ->
      Reply = pollution:get_daily_mean(Type, Day, State),
      From ! {reply, Reply},
      loop(State);

    {call, From, {get_daily_over_limit, Type, Day}} ->
      Reply = pollution:get_daily_over_limit(Type, Day, State),
      From ! {reply, Reply},
      loop(State);

    stop ->
      ok;

    _Other ->
      loop(State)
  end.


call_server(Message) ->
  case whereis(pollution_server) of
    undefined ->
      {error, not_running};
    Pid ->
      Pid ! {call, self(), Message},
      receive
        {reply, Reply} ->
          Reply
      after 5000 ->
        {error, timeout}
      end
  end.