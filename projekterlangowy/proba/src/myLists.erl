%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2025 14:48
%%%-------------------------------------------------------------------
-module(myLists).
-author("agada").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([], _Value) ->
  false;
contains([H | T], Value) ->
  case H =:= Value of
    true -> true;
    false -> contains(T, Value)
  end.

duplicateElements([]) ->
  [];
duplicateElements([H | T]) ->
  [H, H | duplicateElements(T)].

sumFloats(List) ->
  sumFloatsAcc(List, 0.0).

sumFloatsAcc([], Acc) ->
  Acc;
sumFloatsAcc([H | T], Acc) when is_float(H) ->
  sumFloatsAcc(T, Acc + H);
sumFloatsAcc([_NotFloat | T], Acc) ->
  sumFloatsAcc(T, Acc).

