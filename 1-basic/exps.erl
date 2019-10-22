-module(exps).
-export([try_compute/1]).

try_compute({Operation, FirstArg, SecondArg}) ->
	try
		compute({Operation, FirstArg, SecondArg})
	catch
		error:badarith ->
			nil;
		Class:Reason ->
			{Class, Reason}
	end.

compute({minus, FirstArg, SecondArg}) ->
	compute(FirstArg) - compute(SecondArg);

compute({plus, FirstArg, SecondArg}) ->
	compute(FirstArg) + compute(SecondArg);

compute({muliply, FirstArg, SecondArg}) ->
	compute(FirstArg) * compute(SecondArg);

compute({divide, FirstArg, SecondArg}) ->
	compute(FirstArg) - compute(SecondArg);

compute({_, _FirstArg, _SecondArg}) ->
	nil;

compute({num, Arg}) when is_integer(Arg) ->
	Arg;

compute({_,_}) ->
	nil.

