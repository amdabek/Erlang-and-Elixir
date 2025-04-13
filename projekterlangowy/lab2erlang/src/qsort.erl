%%%-------------------------------------------------------------------
%%% @author agada
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. mar 2025 17:07
%%%-------------------------------------------------------------------
-module(qsort).
-author("agada").

%% API
-export([qs/1, grt_eq_than/2, less_than/2]).


less_than([], _Arg) -> [];
less_than(List, Arg) -> [X || X<-List, X<Arg].


grt_eq_than([], _Arg) -> [];
grt_eq_than(List, Arg) -> [X || X<-List, X>=Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

%%random_elems(N, Min, Max) -> [rand:uniform(Max-Min+1)+Min-1 || _<-lists:seq(1,N)].


%%zwracaj (err, dane błędu)



