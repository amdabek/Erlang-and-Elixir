%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. mar 2025 17:03
%%%-------------------------------------------------------------------
-module(pingpong).
-author("agada").

%% API
-export([start/0, stop/0, play/1, ping/0, pong/0]).

start() ->
  register(ping, spawn(pingpong, ping, [])),
  register(pong, spawn(pingpong, pong, [])),
  ok.

stop() ->
  ping ! stop,
  pong ! stop,
  ok.


play(N) ->
  ping ! N,
  ok.


ping() ->
  receive
    0 -> io:format("zero"), ping();
    stop -> io:format("stopuje");
    N -> io:format("Pozostało ~B~n", [N]), pong ! N-1, pong()
    after 20000 -> ok
  end.

pong() ->
  receive
    stop -> io:format("stopuje");
    0 -> io:format("zero"), ping();
    N -> io:format("Pozostało ~B~n", [N]), ping ! N-1, ping()
    after 20000 -> ok
  end.