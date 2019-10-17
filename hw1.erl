-module(hw1).
-export([match/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For now it checks if list 'Structure' matches list 'Pattern' %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
match(Pattern, Structure) -> 
	match(Pattern, Structure, []).

match([PatternHead | Pattern], [StructureHead | Structure], BindedList) when is_list(Pattern), is_list(Structure) ->
	case PatternHead of
		{var, Term} -> 
			case is_binded(BindedList, {bind, Term, StructureHead}) of 
				%% If this term is unbound - we put it in the BindedList
				{ok, new} -> match(Pattern, Structure, [{bind, Term, StructureHead} | BindedList]);
				%% If this term is already bound and it values match - we move forward
				{ok, match} -> match(Pattern, Structure, BindedList);
				%% If values mismatches
				_ -> {false, pattern_mismatch}
			end;
		StructureHead -> match(Pattern, Structure, BindedList);
		_ -> {false, pattern_mismatch}
	end;

%$ When we run through the whole Structure
match([],[], BindedList) -> {true, lists:reverse(BindedList)};

%% When size of the Structure mismatches the Pattern size
match([], _Structure, _BindedList) -> {false, size_mismatch};
match(_Pattern, [], _BindedList) -> {false, size_mismatch};

%% When we got broken lists
match(_,_,_) -> {false, bagargs}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checks if var is already bound %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_binded([Tuple | _Tail], Tuple) -> {ok, match};

is_binded([{bind, Key, _Value} | _Tail], {bind, Key, _NewValue}) -> {error, mismatch};

is_binded([ _Tuple | Tail], Tuple) -> is_binded(Tail, Tuple);

is_binded([], _Tuple) -> {ok, new}.