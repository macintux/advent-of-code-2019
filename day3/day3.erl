-module(day3).
-compile(export_all).

part1(Filename) ->
    {ok, Fh} = file:open(Filename, [read]),
    Line1 = myio:next_line(Fh),
    Line2 = myio:next_line(Fh),
    file:close(Fh),
    Path1 = myio:split_line(Line1),
    Path2 = myio:split_line(Line2),
    AbsPath1 = abs_path(Path1),
    AbsPath2 = abs_path(Path2),
    AbsPathNoTally1 = lists:map(fun({X, Y, _T}) -> {X, Y} end, AbsPath1),
    AbsPathNoTally2 = lists:map(fun({X, Y, _T}) -> {X, Y} end, AbsPath2),
    Intersection = sets:intersection(sets:from_list(AbsPathNoTally1),
                                     sets:from_list(AbsPathNoTally2)),
    Distances = lists:map(fun find_distance/1, sets:to_list(Intersection)),
    lists:min(Distances).

part2(Filename) ->
    {ok, Fh} = file:open(Filename, [read]),
    Line1 = myio:next_line(Fh),
    Line2 = myio:next_line(Fh),
    file:close(Fh),
    Path1 = myio:split_line(Line1),
    Path2 = myio:split_line(Line2),
    AbsPath1 = abs_path(Path1),
    AbsPath2 = abs_path(Path2),
    AbsPathNoTally1 = lists:map(fun({X, Y, _T}) -> {X, Y} end, AbsPath1),
    AbsPathNoTally2 = lists:map(fun({X, Y, _T}) -> {X, Y} end, AbsPath2),
    Intersection = sets:intersection(sets:from_list(AbsPathNoTally1),
                                     sets:from_list(AbsPathNoTally2)),
    Distances1 = find_intersections(Intersection, AbsPath1),
    Distances2 = find_intersections(Intersection, AbsPath2),
    find_shortest(Distances1, Distances2, 10000000).

find_shortest([], [], Shortest) ->
    Shortest;
find_shortest([{X, Y, T1}|Tail1], [{X, Y, T2}|Tail2], Shortest) ->
    find_shortest(Tail1, Tail2, min(Shortest, T1+T2)).

find_intersections(I, P) ->
    Intersections = sets:to_list(I),
    find_tallies(Intersections, P, P, []).

find_tallies([], _Paths, _OrigPaths, Accum) ->
    Accum;
find_tallies([{X, Y}|ITail], [{X, Y, Tally}|_PTail], OrigPaths, Accum) ->
    find_tallies(ITail, OrigPaths, OrigPaths, [{X, Y, Tally}|Accum]);
find_tallies(I, [_H|PTail], OrigPaths, Accum) ->
    find_tallies(I, PTail, OrigPaths, Accum).



find_distance({X, Y}) ->
    abs(X) + abs(Y).

abs_path(Path) ->
    abs_path(Path, {0, 0, 0}, []).

abs_path([], _Last, Path) ->
    Path;
abs_path([H|T], Last, Path) ->
    Steps = move(Last, xlate(H)),
    abs_path(T, lists:last(Steps), Path ++ Steps).

move({X1, Y1, Tally}, {0, Y2}) when Y2 > 0 ->
    lists:map(fun(S) -> {X1, Y1+S, Tally+S} end,
              lists:seq(1, Y2));
move({X1, Y1, Tally}, {0, Y2}) ->
    lists:map(fun(S) -> {X1, Y1-S, Tally+S} end,
              lists:seq(1, abs(Y2)));
move({X1, Y1, Tally}, {X2, 0}) when X2 > 0 ->
    lists:map(fun(S) -> {X1+S, Y1, Tally+S} end,
              lists:seq(1, X2));
move({X1, Y1, Tally}, {X2, 0}) ->
    lists:map(fun(S) -> {X1-S, Y1, Tally+S} end,
              lists:seq(1, abs(X2))).



xlate([$R|T]) ->
    {Int, []} = string:to_integer(T),
    {Int, 0};
xlate([$L|T]) ->
    {Int, []} = string:to_integer(T),
    {-Int, 0};
xlate([$U|T]) ->
    {Int, []} = string:to_integer(T),
    {0, Int};
xlate([$D|T]) ->
    {Int, []} = string:to_integer(T),
    {0, -Int}.
