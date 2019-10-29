-module (paral).
-compile(export_all).


map_collect([]) ->
	[];

map_collect([NextPid | Tail]) ->
	Result = 
	receive
		{result, NextPid, X} -> X;
		{'EXIT', NextPid, Reason} -> {error, Reason}
	end,
	[Result | map_collect(Tail)].

map(Func, List, PartialResult) ->
	SpawnProc = parallel(Func, PartialResult),
	Pids = [SpawnProc(X) || X <- List],
	map_collect(Pids).

test_map() ->
	map(fun(X) -> 10*X end, [1, 2, 3, 7], true).

foldl(Func, List, PartialResult) ->
	PairList = sep_list(List),
	SpawnProc = parallel(fix_function(Func), PartialResult),
	Pids = [SpawnProc(X) || X <- PairList],
	IterList = foldl_collect(Pids),
	case IterList of
		[Result] -> Result;
		_ -> foldl(Func, IterList, PartialResult)
	end.

foldl_collect([NextPid | Tail]) ->
	Result = 
	receive
		{result, NextPid, X} -> X;
		{'EXIT', NextPid, Reason} -> {error, Reason}
	end,
	[Result | foldl_collect(Tail)];

foldl_collect([]) ->
	[].

fix_function(Func) ->
	fun([A, B]) ->
		case B of
			nil ->
				A;
			_ ->
				Func([A,B])
		end
	end.

test_foldl() ->
	map(fun(X) -> 10*X end, [1, 2, 3, 7], true).

sep_list(List) ->
	lists:reverse(sep_list(List, [])).

sep_list([H1 |[H2 | Tail]], Acc) ->
	sep_list(Tail, [[H1, H2] | Acc]);

sep_list([H1 | []], Acc) ->
	[[H1, nil] | Acc];

sep_list([], Acc) ->
	Acc.

parallel(Func, PartialResult) ->
	Self = self(),
	process_flag(trap_exit, PartialResult),
	fun(X) ->
		spawn_link(fun() ->
			Result = Func(X),
			Self ! {result, self(), Result}
		end)
	end.