-module(mylists).
-export([create/1, create_reversed/1, create_and_print/1, create_and_print_odd/1, filter/2, reverse/1, concatenate/1, flatten/1, dna_to_rna/1, cut_rdna/2]).

create(N) -> lists:seq(1,N).

create_reversed(N) -> reverse(create(N)).

create_and_print([]) -> ok;

create_and_print([Head | Tail]) ->
	io:fwrite("~p~n", [Head]),
	create_and_print(Tail);

create_and_print(N) -> create_and_print(create(N)).

create_and_print_odd([]) -> ok;

create_and_print_odd([Head | Tail]) ->
	io:fwrite("~p~n", [Head]),
	create_and_print(Tail);

create_and_print_odd(N) -> create_and_print_odd([X || X <- create(N), X rem 2 == 1]).

filter(List, Value) -> filter(List, Value, []).

filter([], _Value, Acc) -> reverse(Acc);

filter([Head | Tail], Value, Acc) when is_list(Tail) ->
	if
		Head =< Value ->
			filter(Tail, Value, [Head | Acc]);
		true -> filter(Tail, Value, Acc)
	end;

filter(_,_,_) -> badarg.

reverse(List) -> reverse(List, []).

reverse([], Acc) -> Acc;

reverse([Head | Tail], Acc) when is_list(Tail) ->
	reverse(Tail, [Head | Acc]);

reverse(_,_) -> badarg.

concatenate(List) -> reverse(concatenate(List, [])).

concatenate([], Acc) -> Acc;

concatenate([Head | Tail], Acc) when is_list(Tail) ->
	if
		is_list(Head) ->
			NewAcc = concatenate(Head, Acc),
			concatenate(Tail, NewAcc);
		true -> 
			concatenate(Tail, [Head | Acc])
	end;

concatenate(_,_) -> badarg.

flatten(List) -> concatenate(List).

dna_to_rna(List) -> reverse(dna_to_rna(List, [])).

dna_to_rna([], Acc) -> Acc;

dna_to_rna([Head | Tail], Acc) when is_list(Tail) ->
	case Head of
		a -> dna_to_rna(Tail, [u | Acc]);
		g -> dna_to_rna(Tail, [c | Acc]);
		c -> dna_to_rna(Tail, [g | Acc]);
		t -> dna_to_rna(Tail, [a | Acc]);
		65 -> dna_to_rna(Tail, [u | Acc]);
		71 -> dna_to_rna(Tail, [c | Acc]);
		67 -> dna_to_rna(Tail, [g | Acc]);
		84 -> dna_to_rna(Tail, [a | Acc]);
		_ -> badarg
	end;

dna_to_rna(_, _) -> badarg.

cut_rdna(List, Seq) -> List -- Seq.