Func = fun Loop([{dimension, _N}, {dotsA, List1}, {dotsB, List2}]) ->
	F = fun Loop([H | T], [H1 | T1], Operation) ->
		case T ++ T1 of
			[] ->
				[Operation(H, H1)];
			T ->
				[H | Loop(T, T1, Operation)];
			T1 -> 
				[H1 | Loop(T, T1, Operation)];
			_ ->
				[Operation(H, H1) | Loop(T, T1, Operation)]
			end
		end,
	F2 = fun Loop([H | T]) ->
		case T of
			[] ->
				H*H;
			_ ->
				H*H + Loop(T)
		end
	end,
	[math:sqrt(F2(F(tuple_to_list(A), tuple_to_list(B), fun(X, Y) -> X - Y end))) || A <- List1, B <- List2]
end.


F = fun Loop([H | T], [H1 | T1], Operation) ->
		case T ++ T1 of
			[] ->
				[Operation(H, H1)];
			T ->
				[H | Loop(T, T1, Operation)];
			T1 -> 
				[H1 | Loop(T, T1, Operation)];
			_ ->
				[Operation(H, H1) | Loop(T, T1, Operation)]
			end
		end.

%% [{dimension, 5}, {dotsA, [{1, 2, 3, 4, 5}, {7, 8, 9, 10, 11}]}, {dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}]