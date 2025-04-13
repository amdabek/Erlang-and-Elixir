%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2025 14:36
%%%-------------------------------------------------------------------
-module(hello).
-author("agada").

%% API
-export([hello_world/0]).

hello_world() ->
  io:format("Hello World~n").
