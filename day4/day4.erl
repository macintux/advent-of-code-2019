-module(day4).
-compile(export_all).

is_valid(I) ->
    Ints = breakdown(I),
    case lists:sort(Ints) /= Ints of
        true ->
            false;
        _ ->
            scan(Ints, -1, 0)
    end.

scan([], _, 1) ->
    true;
scan([], _, _N) ->
    false;
scan([H], H, 0) ->
    true;
scan([H], H, _N) ->
    false;
scan([_H], _SomethingElse, 0) ->
    false;
scan([_H], _SomethingElse, 1) ->
    true;
scan([H|T], H, 0) ->
    scan(T, H, 1);
scan([H|T], H, N) ->
    scan(T, H, N+1);
scan([_H|_T], _SomethingElse, 1) ->
    true;
scan([H|T], _SomethingElse, _N) ->
    scan(T, H, 0).

is_valid({false, _, _}, _) ->
    false;
is_valid({true, true, -1}, [_]) ->
    false;
is_valid({true, true, _}, [_]) ->
    true;
is_valid({true, Found, ShortDup}, [H|T]) ->
    is_valid(check_each(H, T, Found, ShortDup), T).

check_each(X, [X|_T], _Found, X) ->
    {true, false, X};
check_each(X, [Y|_T], _Found, X) when Y > X ->
    {true, true, X};
check_each(X, [Y|_T], Found, _Last) when Y > X ->
    {true, Found, X};
check_each(X, _, Found, _Last) ->
    {false, Found, X}.

breakdown(Val) ->
    breakdown(Val, []).

breakdown(Val, Accum) when Val =< 0 ->
    Accum;
breakdown(Val, Accum) ->
    breakdown(Val div 10, [Val rem 10|Accum]).


%% No longer works because I added the part 2 logic directly to
%% is_valid/1
part1(Lower, Upper) ->
    length(lists:filter(fun is_valid/1, lists:seq(Lower, Upper))).

%% My inputs: 248345, 746315
part2(Lower, Upper) ->
    length(lists:filter(fun is_valid/1, lists:seq(Lower, Upper))).
