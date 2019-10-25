# Блок "Обзор встроенных библиотек языка"

[Оригинал заданий](https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/2-advanced.md)

### Упражнения

#### 2.0. База данных

> Напишите модуль db.erl, создающий базу данных, в которой можно хранить,
> записывать и удалять элементы.  Функция `destroy/1` удаляет базу данных. Сборщик
> мусора выполнит всю работу за вас. Но в том случае, если база хранится в файле,
> при вызове функции `destroy` вам придётся удалить этот файл. Функция `destroy`
> включена для полноты интерфейса. При выполнении этого упражнения функциями из
> модулей *[lists](http://erlang.org/doc/man/lists.html)*,
> *[proplists](http://erlang.org/doc/man/proplists.html)*,
> *[dict](http://erlang.org/doc/man/dict.html)* пользоваться нельзя. Все
> рекурсивные функции должны быть написаны вами. Подсказка: используйте списки и
> кортежи в качестве основного типа данных. При тестировании помните, что все
> переменные могут связываться со значением только один раз.
> 
> Интерфейс:
> ```erlang
> db:new() -> Db.
> db:destroy(Db) -> ok.
> db:write(Key, Element, Db) -> NewDb.
> db:delete(Key, Db) -> NewDb.
> db:read(Key, Db) -> {ok, Element} | {error, instance}.
> db:match(Element, Db) -> [Keyl, ..., KeyN].
> ```
> 
> Пример использования в интерпретаторе:
> ```erlang
> 1> c(db).
> {ok,db}
> 2> Db = db:new().
> []
> 3> Dbl = db:write(francesco, london, Db).
> [{francesco,london}]
> 4> Db2 = db:write(lelle, 'Stockholm', Dbl).
> [{lelle,'Stockholm'},{francesco,london}]
> 5> db:read(francesco, Db2).
> {ok,london}
> 6> Db3 = db:write(joern, 'Stockholm', Db2).
> [{joern,'Stockholm'}, {lelle,'Stockholm'}, {francesco,london}]
> 7> db:read(ola, Db3).
> {error,instance}
> 8> db:match('Stockholm', Db3).
> [joern,lelle]
> 9> Db4 = db:delete(lelle, Db3).
> [{joern,'Stockholm'}, {francesco,london}]
> 10> db:match('Stockholm', Db4).
> [joern]
> ```
 
[db.erl](https://github.com/ArtamonovAlex/Reksoft_tasks/blob/master/2-advanced/db.erl) - Done

### 2.1 Новая функциональность в базе данных

> Реализуйте следующие возможности в нашей базе данных.
> 
> Интерфейс:
> ```erlang
> Parameters = [Opt | Parameters].
> Opt = {append, allow|deny} | {batch, Number :: non_neg_integer()}.
> KeyList = [Key1,...KeyN].
> 
> db:new(Parameters) -> Db.
> db:append(Key, Element, Db) -> NewDb.
> db:batch_delete(KeyList, Db) -> NewDb | {error, batch_limit}.
> db:batch_read(KeyList, Db) -> [{Key, Element}] | {error, instance} | {error, batch_limit}.
> ```

[db.erl](https://github.com/ArtamonovAlex/Reksoft_tasks/blob/master/2-advanced/db.erl) - In progress

### 2.2 Сделайте так, чтобы база данных работала с JSON объектами, реализованными ранее в п.1.8

[db.erl](https://github.com/ArtamonovAlex/Reksoft_tasks/blob/master/2-advanced/db.erl) - In progress

### 2.3 Lambda-вычисления

> Реализуйте следующие функции, используя рекурсию:
> * Напишите lambda-функцию, которая осуществляет произвольную операцию
>   ``Operation(A, B) -> C`` (где ``A, B, C`` - числа), над двумя числовыми
>   списками попарно, возвращая список результатов операции также в виде списка.
>   Проверьте вашу функцию на разных операциях (``erlang:'+'``, ``erlang:'xor'``,
>   ``erlang:'rem'``, ``erlang:'/'`` и собственной фунции, которая возвращает
>   среднее гармоническое двух чисел ``H = 2/(1/A + 1/B)``).
> * Напишите lambda-функцию, которая для каждой точки точки из списка ``dotsA``
>   вычисляет расстояние до всех точек из списка точек ``dotsB`` в пространстве
>   размерности N.  Напишите функцию, которая читает следующую нотацию:
> 
> ```erlang
> [
>     {dimension, 5},
>     {dotsA, [{1, 2, 3, 4 5}, {7, 8, 9, 10, 11}]},
>     {dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}
> ]
> ```
> 
> и возвращает:
> [ 5.360220495669696, 10.720440991339393, 12.988650063170537, 18.14700750425752 ]

[lambda.erl](https://github.com/ArtamonovAlex/Reksoft_tasks/blob/master/2-advanced/lambda.erl) - In progress

### 2.4 Библиотечные функции

> Реализуйте следующие те же самые lambda-функции, реализованные в п.2.3,
> используя библиотечные функции: ``foldl``, ``map``
> 
> Дополнительно:
> * Реализуйте собственную функцию `my_lists:filtermap` через `lists:foldl`.
>   Синтаксис `my_lists:filtermap` должен совпадать с синтаксисом
>   `lists:filtermap`

[my_lists.erl](https://github.com/ArtamonovAlex/Reksoft_tasks/blob/master/2-advanced/my_lists.erl) - In progress

### 2.5 Ленивые вычисления

#### 2.5.1 I'm lazy...

>  - Реализуйте ленивые версии функций map, filter, foldl. Назовите эти функции
>    `lazy_map`, `lazy_foldl`, `lazy_filter`. Покажите, что в общем случае
>    правосторонняя свертка невозможна для ленивых списков. Получится ли выразить
>    функции `lazy_map` и `lazy_filter` через `laxy_foldl`? Почему?
>  - Реализуйте конкатенацию ленивых списков. В чем особая польза этой функции?
>  - Реализуйте ленивое чтение из файла. Воспользуйтесь функциями `file:open/2`,
>    `file:read/2`. Ленивый поток является абстракцией последовательности строк -
>    то есть каждый вызов отложенного вычисления выдает следующую строку файла.
>  - В файле лежит последовательность чисел (для простоты предположим, что одна
>    строка - это одно число). Необходимо вычислить максимальную сумму непрерывной
>    подпоследовательности данных чисел. Файл должен обрабатываться с помощью
>    ленивых списков. Если исходная последовательность пустая, то считаем, что
>    сумма равна нулю. Рассмотрим несколько примеров:
>     - если последовательность чисел полностью состоит из положительных чисел, то
>       такая сумма является суммой всех чисел в последовательности.
>     - если последовательность чисел полностью состоит из отрицательных чисел, то
>       максимальная сумма подпоследовательности равна нулю, так как в данном
>       случае подпоследовательность является пустой.
>   Нужно найти такую непрерывную подпоследовательность исходной
>   последовательности, сумма которой будет максимальна.
>  - То же, что и предыдущий пункт, но с дополнительными условями:
>    - необходимо возвращать саму подпоследовательность;
>    - добавить ограничение: минимальная длина подпоследовательности должна быть
>      K.

[lazy.erl](https://github.com/ArtamonovAlex/Reksoft_tasks/blob/master/2-advanced/lazy.erl) - In progress
