-module (lazy).

-compile(export_all).



lazy_map(_Fun, []) ->
	fun() ->
		[]
	end;

lazy_map(Fun, [H | T]) ->
	fun() ->
		[Fun(H) | lazy_map(Fun, T)]
	end.

lazy_foldl(Fun, Acc, [H | T]) when T /= [] ->
	fun() ->
		TempResult = Fun(H, Acc),
		[TempResult | lazy_foldl(Fun, TempResult, T)]
	end;

lazy_foldl(Fun, Acc, [LastElem]) ->
	fun() ->
		[Fun(LastElem, Acc)]
	end.

filter(Fun, [H | T]) ->
	case Fun(H) of
		true ->
			[H | T];
		false ->
			filter(Fun, T)
	end.

lazy_filter(_Fun, []) ->
	[];

lazy_filter(Fun, List) ->
	fun() ->
		[H | T] = filter(Fun, List),
		[H | lazy_filter(Fun, T)]
	end.

collect_lazy_list(LazyList) when is_function(LazyList) ->
	[H | T] = LazyList(),
	[H | collect_lazy_list(T)];
	
collect_lazy_list(LastElem) ->
		LastElem.

open_file(Filename) ->
	{ok, IODevice} = file:open(Filename, [read]),
	IODevice.

lazy_read(IODevice) ->
	fun() ->
		case file:read_line(IODevice) of
			{ok, Data} ->
				Data -- "\n";
			eof ->
				[]
		end
	end.