# Reksoft_tasks

Homework for Reksoft Erlang Academy

## Homework_1

We need to write a function `match(Pattern, DataStructure)` that returns `{false, Reason}` if DataStructure mismatches the Pattern and `{true, BindedList}` if it matches.
Pattern looks like  `[{var, a}, {var, b}, 3]` where *a* and *b* are unbound atoms. BindedList consist of tuples like `{bind, Atom, Value}` where Atom is unbound atoms from pattern and Value is corresponding value from DataStructure.
### Examples
DataStructure `[1,2,3]` completely matches pattern `[{var, a}, {var, b}, 3]`, BindedList: `[{bind, a, 1}. {bind, b, 2}]`

DataStructure `[1,1,3]` completely matches pattern `[{var, a}, {var, a}, 3]`, BindedList: `[{bind, a, 1}]`

DataStructure `[1,2,3]` mismatches pattern `[{var, a}, {var, a}, 3]`

DataStructure `[1,2,3]` completely matches pattern `[1, 2, 3]`, BindedList: `[ ]`

## Homework_2

We need to write a function `get_values(List, Key)` that get a list of key-value structures and a Key and returns a list of all values with such Key in the list
