%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2025 14:42
%%%-------------------------------------------------------------------
-module(pow).
-author("agada").

%% API
-export([power/2]).

power(_X, 0) ->
  1;
power(X, N) when N > 0 ->
  X * power(X, N - 1);
power(X, N) when N < 0 ->
  1 / power(X, -N).
