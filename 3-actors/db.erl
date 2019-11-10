-module (db).
-export([
	new/0,
	destroy/1,
	write/3,
	delete/2,
	read/2,
	match/2
	]).

new() ->
	spawn(fun() ->
		db_proc([])
	end).

db_proc(State) ->
	NewState = receive
		{write, From, Key, Value} ->
			From ! ok,
			write_internal(Key, Value, State);
		{delete, From, Key} ->
			From ! ok,
			delete_internal(Key, State);
		{read, From, Key} -> 
			From ! read_internal(Key, State),
			State;
		{match, From, Key} ->
			From ! match_internal(Key, State),
			State
	end,
	db_proc(NewState).


destroy(Db) ->
	exit(Db, exit).

write_internal(Key, Value, Db) ->
	NewDb = delete_internal(Key, Db),
	[{Key, Value} | NewDb].

delete_internal(Key, Db) ->
	delete_internal(Key, Db, []).

delete_internal(Key, [{Key, _Value} | Db], Acc) ->
	Db ++ Acc;

delete_internal(Key, [Element | Db], Acc) ->
	delete_internal(Key, Db, [Element | Acc]);

delete_internal(_Key, [], Acc) ->
	Acc.

read_internal(Key, [{Key, Value} | _Db]) -> 
	{ok, Value};

read_internal(Key, [_Element | Db]) -> 
	read_internal(Key, Db);

read_internal(_Key, []) -> 
	{error, instance}.

match_internal(Element, Db) ->
	match_internal(Element, Db, []).

match_internal(Element, [{Key, Element} | Db], Acc) ->
	match_internal(Element, Db, [Key | Acc]);

match_internal(Element, [_Element | Db], Acc) ->
	match_internal(Element, Db, Acc);

match_internal(_Element, [], Acc) ->
	Acc.

write(Key, Value, Db) ->
	case erlang:is_process_alive(Db) of
		true ->
			Db ! {write, self(), Key, Value},
			receive
				Msg -> Msg
			end;
		false ->
			{error, "Database is down"}
	end.

delete(Key, Db) ->
	case erlang:is_process_alive(Db) of
		true ->
			Db ! {delete, self(), Key},
			receive
				Msg -> Msg
			end;
		false ->
			{error, "Database is down"}
	end.

read(Key, Db) ->
	case erlang:is_process_alive(Db) of
		true ->
			Db ! {read, self(), Key},
			receive
				Msg -> Msg
			end;
		false ->
			{error, "Database is down"}
	end.

match(Key, Db)->
	case erlang:is_process_alive(Db) of
		true ->
			Db ! {match, self(), Key},
			receive
				Msg -> Msg
			end;
		false ->
			{error, "Database is down"}
	end.