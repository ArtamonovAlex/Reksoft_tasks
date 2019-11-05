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

## Lockman

> Доработайте модуль lockman.erl так, чтобы он как то умел справляться с ситуациями, когда клиентские процессы умирают

Необходимо сначала создать монитор который будет следить за всеми процессами, вызывающими acquire; затем добавляем сразу в вызовы
функций acquire и release *erlang:monitor* и *erlang:demonitor* соответственно; 
Если мониторинговый процесс умирает, отправляем gen_server'у сообщение о том, что он умер, а в gen_server'е обрабатываем это
сообщение следующим путём - делаем всё так же, как в release, только освобождаем не по ключу, а по пиду.  

[lockman.erl]()

Но на мой взгляд было бы элегантнее определить функцию *do_work()*, тело которой бы было примерно такого содержания:
```erlang
do_work(LockManager, Source, Task) ->
    P = spawn(fun() ->
    lockman:acquire(LockManager, Source),
    Task(Source),
    lockman:release(LockManager, Source)
    end),
    Ref = erlang:monitor(process, P),
    receive
        {'DOWN', Ref, process, P, normal} ->
            ok;
        {'DOWN', Ref, process, P, Reason} ->
            gen_server:cast(LockManager, {proc_down, P}),
            {error, Reason}
    end.
```
Так мы не напрягаем функции acquire и release лишней логикой, следим только за процессом между acquire и release. Но в
исходном модуле мы не вводили понятие таска и его выполнения, поэтому пришлось начинать мониторинг процесса в функции acquire.
