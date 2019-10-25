-module (db).
-export([
	new/0,
	destroy/1,
	write/3,
	delete/2,
	read/2,
	test/0,
	match/2,
	fill_db/0
	]).

new() ->
	[].

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




fill_db() ->
	Db = [] = new(),
	Db1 = write(person1,{john, smith},Db),
	Db2 = write(person2,{john, smith},Db1),
	write(person3,{william, smith},Db2).

test() ->
	{test_write(), test_delete(), test_read(), test_match()}.

test_delete() ->
	try
		Db = fill_db(),
		[{person1,{john,smith}},{person3,{william,smith}}] = delete(person2, Db),
		Db = lists:reverse(delete(test, Db)),
		[] = delete(test, []),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.

test_read() ->
	try
		Db = fill_db(),
		{ok,{william,smith}} = read(person3, Db),
		{ok,{john,smith}} = read(person1, Db),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.

test_write() ->
	try
		Db = [] = new(),
		Db1 = [{person1, {john, smith}}] = write(person1,{john, smith},Db),
		Db2 = [{person2,{john,smith}},{person1,{john,smith}}] =  write(person2,{john, smith},Db1),
		[{person3,{william,smith}}, {person1,{john,smith}}, {person2,{john,smith}}] = write(person3,{william, smith},Db2),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.

test_match() ->
	try
		Db = fill_db(),
		[person2, person1] = match({john,smith}, Db),
		[person3] = match({william,smith}, Db),
		[] = match(test, Db),
		ok
	catch
		Class:Reason -> {Class, Reason}
	end.