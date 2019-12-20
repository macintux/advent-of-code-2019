-module(polar).
-compile(export_all).

%% This is sufficient for coordinates up to 1 million (Cartesian).
%% > polar:same_line(polar:c2p({1000000, 1000000}), polar:c2p({1000000, 1000001})).
%% false
-define(DELTA, 0.0000001).

%% For grouping polar angles, again up to 1 million (Cartesian).
-define(INT_FACTOR, 10000000).

%% To make sure we don't accidentally call the wrong function, throw a
%% 'p' atom as a tag for polar coordinates.

%% For our problem space, we're only interested in integer Cartesian
%% coordinates.
p2c({p, R, T}) ->
    {round(R * math:cos(T)),
     round(R * math:sin(T))}.

c2p({0, 0}) ->
    origin; %% ?
c2p({X, Y}) ->
    {p,
     math:sqrt((X*X) + (Y*Y)),
     atan2(X, Y)}.

%% Only matches same vector, essentially. Line away from the origin in
%% one direction only.
same_line({p, _R1, T1}, {p, _R2, T2}) when abs(T1 - T2) < ?DELTA ->
    true;
same_line(_P1, _P2) ->
    false.

%% Thanks, Wikipedia
atan2(0, 0) ->
    origin;
atan2(0, Y) when Y > 0 ->
    math:pi() / 2;  %% + pi/2
atan2(0, Y) when Y < 0 ->
    -atan2(0, -Y); %% + pi/2
atan2(X, Y) when X > 0 ->
    math:atan(Y / X); %% + pi/2
atan2(X, Y) when X < 0, Y >= 0 ->
    math:atan(Y / X) + math:pi(); %% atan2(-Y, X) ?
atan2(X, Y) when X < 0, Y < 0 ->
    math:atan(Y / X) - math:pi(). %% + pi/2

%% I hate dealing with floating point numbers. Convert our angles to
%% large integers. I suspect it doesn't buy me anything but it makes
%% me feel better.
int_angle({p, R, T}) ->
    {theta_int, R, round(T*?INT_FACTOR)}.


group_polar(origin, Dict) ->
    Dict;
group_polar({p, R, _T}=P, Dict) ->
    {theta_int, R, T_I} = int_angle(P),
    dict:update(T_I, fun(Acc) -> [P|Acc] end,
                [P], Dict).

sort_polar({p, R1, _T1}, {p, R2, _T2}) ->
    R1 < R2.

drop_nearest(_Key, PolarCoords, Acc) ->
    Acc ++ lists:nthtail(1, lists:sort(fun sort_polar/2, PolarCoords)).

%% Organization on these modules is ugly and running group_polar/2 an
%% extra time unnecessarily is lazy but I want to move on with other
%% problems.
laser_sort(PolarCoords) ->
    PDict = lists:foldl(fun group_polar/2, dict:new(), PolarCoords),
    SortedThetas = lists:reverse(lists:sort(dict:fetch_keys(PDict))),
    follow_laser(SortedThetas, SortedThetas, PDict, []).

%% Very simple: step through our theta angles clockwise, over and over
%% again until all asteroids have been "blown up".
follow_laser([], SortedThetas, Dict, Accum) ->
    case dict:is_empty(Dict) of
        true ->
            lists:reverse(Accum);
        false ->
            extract_laser(SortedThetas, SortedThetas, Dict, Accum)
    end;
follow_laser([H|T], SortedThetas, Dict, Accum) ->
    {Coords, Dict1} = dict:take(H, Dict),
    Coords2 = lists:reverse(Coords),
    case Coords2 of
        [LastVal] ->
            extract_laser(T, SortedThetas -- [H], Dict1, [LastVal|Accum]);
        [Val|Rest] ->
            extract_laser(T, SortedThetas, dict:store(H, lists:reverse(Rest), Dict1),
                          [Val|Accum])
    end.


%% > Coords = [{1, 1}, {1, 2}, {2, 2}, {3, 3}, {-1, -1}].
%% > lists:map(fun polar:p2c/1, polar:occluded(lists:map(fun polar:c2p/1, Coords))).
%% [{2,2},{3,3}]
occluded(PolarCoords) ->
    dict:fold(fun drop_nearest/3, [],
              lists:foldl(fun group_polar/2, dict:new(), PolarCoords)).

%% Essentially the inverse of occluded/1.
visible(PolarCoords) ->
    dict:size(lists:foldl(fun group_polar/2, dict:new(), PolarCoords)).
