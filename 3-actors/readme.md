# Блок "Модель акторов, процессы"

[Оригинал заданий](https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/3-actors.md)

### Упражнения

### 3.1. База данных

> За основу возьмите модуль db из предыдущего задания, оформите это в виде
> процесса. API примерно такой же, с небольшими изменениями:
> 
> ```erlang
> db:new() => Pid.  %% Возвращает pid созданного процесса.
> ```
> 
> ```erlang
> db:destroy(Db) => ok.
> ```
> Убивает созданный процесс с помощью функции exit(Pid, Reason),
> `Pid` - идентификатор процесса,
> `Reason` - терм, обозначающий причину прерывания.
> В качестве Reason можно указать атом exit.
> 
> ```erlang
> db:write(Key, Element, Db) => ok | {error, Reason}. 
> ```
> 
> В качестве ошибочной ситуации можно привести пример, когда процесс базы данных
> уже убит, а мы пытаемся все еще что-то с ним делать. Для проверки, жив ли
> процесс или уже нет, можно использовать функцию `is_process_alive(Pid)`.
> 
> ```erlang
> db:delete(Key, Db) => ok | {error, Reason}.
> db:read(Key, Db) => {ok, Element} | {error, instance}.
> db:match(Element, Db) => [Keyl, ..., KeyN].
> ```
> 
> Каждый процесс может иметь состояние, вспоминайте хвостовую рекурсию.
> Воспользуйтесь идеей, приведенной ниже:
> 
> ```erlang
> proc(State) ->
>     NewState = receive
>         Msg ->
>             %% ...какие то вычисления с термами Msg и State,
>             %% возвращающие новое состояние NewState...
>     end,
>     proc(NewState).
> ```

db.erl - In progress

### 3.2. Parallelisation

> ```erlang
> -module(pmap).
> -compile(export_all).
> 
> parallel(Funs) ->
>     Self = self(),
>     SpawnProc = fun(Func) ->
>         spawn(fun() ->
>             Result = Func(),
>             Self ! {self(), Result}
>         end)
>     end,
>     Pids = [SpawnProc(Fun) || Fun <- Funs],
>     get_result(Pids, []).
> 
> get_result([], Acc) ->
>     lists:reverse(Acc);
> get_result([First|Pids], Acc) ->
>     receive
>         {First, Result} ->
>             get_result(Pids, [{First, Result}|Acc])
>     end.
> ```
> 
> На основе вышеприведенного примера реализуйте следующее:
>  - параллелизованную версию функции `map`;
>  - параллелизованную версию функции `foldl`. У такой версии функции будут
>    достаточно весомые орграничения в применении, она уже не будет достаточно
>    универсальной по сравнению с обычной функцией свертки. Что это за
>    ограничения?
>  - скомбинируйте решения и реализуйте простой map/reduce.

[paral.erl](https://github.com/ArtamonovAlex/Reksoft_tasks/blob/master/3-actors/paral.erl) - Done