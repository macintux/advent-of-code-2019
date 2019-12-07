-module(day1).
-compile(export_all).

fuel(Mass) ->
    (Mass div 3) - 2.

real_fuel(Mass) ->
    real_fuel(0, fuel(Mass)).

real_fuel(Total, Next) when Next =< 0 ->
    Total;
real_fuel(Total, Next) ->
    real_fuel(Total+Next, fuel(Next)).

part1(Filename) ->
    lists:sum(lists:map(fun fuel/1, myio:all_integers(Filename))).

part2(Filename) ->
    lists:sum(lists:map(fun real_fuel/1, myio:all_integers(Filename))).
