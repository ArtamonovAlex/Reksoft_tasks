-module(hw2).
-export([get_values/2]).

get_values(List, Key) ->
	get_values(List, Key, []).

get_values([], _Key, []) -> nil;

get_values([], _Key, Acc) -> lists:reverse(Acc);

get_values([_Element | Tail], _Key, _Acc) when not is_list(Tail) -> bagarg;

get_values([{Key, Value} | Tail], Key, Acc) ->
	get_values(Tail, Key, [Value | Acc]);

get_values([_Element | Tail], Key, Acc) ->
	get_values(Tail, Key, Acc).