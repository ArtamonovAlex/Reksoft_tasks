-module (db).
-export([
	new/1,
	append/3,
	destroy/1,
	write/3,
	delete/2,
	batch_read/2,
	batch_delete/2,
	read/2,
	test/0,
	match/2,
	fill_db/1
	]).

new(Parameters) ->
	new(Parameters, []).

new([], Db) ->
	Db;

new([{append, allow} | Parameters], Db) ->
	NewDb = write(append, allow, Db),
	new(Parameters, NewDb);

new([{append, deny} | Parameters], Db) ->
	NewDb = write(append, deny, Db),
	new(Parameters, NewDb);

new([{batch, Value} | Parameters], Db) ->
	if
		Value < 0 ->
			{error, badarg};
		true ->
			NewDb = write(batch, Value, Db),
			new(Parameters, NewDb)
	end;

new([{_Key, _Value} | _Parameters], _Db) ->
	{error, badarg}.

append(append, _Value, _Db) ->
	{error, denied};

append(batch, _Value, _Db) ->
	{error, denied};

append(Key, Element, Db) ->
	case read(append, Db) of
		{ok, allow} ->
			write(Key, Element, Db);
		{ok, deny} ->
			{error, denied};
		{error, instance} ->
			write(Key, Element, Db);
		_ ->
			{error, unknown}
	end.

destroy(_Db) ->
	ok.



write(Key, Value, Db) ->
	NewDb = delete(Key, Db),
	[{Key, Value} | NewDb].

delete(Key, Db) ->
	delete(Key, Db, []).

delete(Key, [{Key, _Value} | Db], Acc) ->
	Db ++ Acc;

delete(Key, [Element | Db], Acc) ->
	delete(Key, Db, [Element | Acc]);

delete(_Key, [], Acc) ->
	Acc.

read(Key, [{Key, Value} | _Db]) -> 
	{ok, Value};

read(Key, [_Element | Db]) -> 
	read(Key, Db);

read(_Key, []) -> 
	{error, instance}.

match(Element, Db) ->
	match(Element, Db, []).

match(Element, [{Key, Element} | Db], Acc) ->
	match(Element, Db, [Key | Acc]);

match(Element, [_Element | Db], Acc) ->
	match(Element, Db, Acc);

match(_Element, [], Acc) ->
	Acc.


batch_read(KeyList, Db) ->
	case read(batch, Db) of
		{error, instance} ->
			batch_read(KeyList, Db, [], -1);
		{ok, Value} -> batch_read(KeyList, Db, [], Value)
	end.

batch_read([], _Db, [], _BatchCounter) ->
	{error, instance};

batch_read([], _Db, Acc, _BatchCounter) ->
	Acc;

batch_read([Key | List], Db, Acc, BatchCounter) ->
	case read(Key, Db) of
		{ok, Value} ->
			if
				BatchCounter == 0 ->
					{error, batch_limit};
				true ->
					batch_read(List, Db, [{Key, Value} | Acc], BatchCounter - 1)
			end;
		{error, instance} -> batch_read(List, Db, Acc, BatchCounter)
	end.


batch_delete(KeyList, Db) ->
	case PresentedElements = batch_read(KeyList, Db) of
		{error, batch_limit} -> {error, batch_limit};
		{error, instance} -> Db;
		_ ->
			PresentedKeys = [X || {X, _Value} <- PresentedElements],
			lists:foldl(fun(X, AccIn) -> delete(X, AccIn) end, Db, PresentedKeys)
	end.

fill_db(Db) ->
	Db1 = write(person1,{john, smith},Db),
	Db2 = write(person2,{john, smith},Db1),
	write(person3,{william, smith},Db2).

test() ->
	{{write, test_write()}, {delete, test_delete()}, {read, test_read()}, {match, test_match()}}.

test_delete() ->
	try
		Db = fill_db(new([])),
		[{person1,{john,smith}},{person3,{william,smith}}] = delete(person2, Db),
		Db = lists:reverse(delete(test, Db)),
		[] = delete(test, []),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.

test_read() ->
	try
		Db = fill_db(new([])),
		{ok,{william,smith}} = read(person3, Db),
		{ok,{john,smith}} = read(person1, Db),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.

test_write() ->
	try
		Db = [] = new([]),
		Db1 = [{person1, {john, smith}}] = write(person1,{john, smith},Db),
		Db2 = [{person2,{john,smith}},{person1,{john,smith}}] =  write(person2,{john, smith},Db1),
		[{person3,{william,smith}}, {person1,{john,smith}}, {person2,{john,smith}}] = write(person3,{william, smith},Db2),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.

test_match() ->
	try
		Db = fill_db(new([])),
		[person2, person1] = match({john,smith}, Db),
		[person3] = match({william,smith}, Db),
		[] = match(test, Db),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.