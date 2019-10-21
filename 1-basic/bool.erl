-module(bool).
-export([b_not/1, b_and/2, b_or/2, b_xor/2]).

b_not(false) -> true;
b_not(_) -> false.

b_and(true, true) -> true;
b_and(_, _) -> false.

b_or(true, _) -> true;
b_or(_, true) -> true;
b_or(_, _) -> false.

b_xor(true, false) -> true;
b_xor(false, true) -> true;
b_xor(_, _) -> false.