-module(day11).
-compile(export_all).

-include("intcode.hrl").

camera_input(Robot, ColorAtom) ->
    intcode:execute(continue, intcode:add_input(Robot, color_int(ColorAtom))).

part1(Filename) ->
    Robot = intcode:load(Filename),
    Surface = matrix:initialize(black),
    Start = {500, 500, up}, %% X, Y, Direction
    {PaintedSet, _PaintedSurface} =
        run_robot(Robot, Start, matrix:get_value(coords(Start), Surface),
                  sets:new()),
    sets:size(PaintedSet).

part2(Filename) ->
    Robot = intcode:load(Filename),
    Start = {500, 500, up}, %% X, Y, Direction
    Surface = matrix:initialize(black),
    %% I could modify Surface to paint the starting point white, but
    %% instead I'll just lie about the camera input. The robot is
    %% going to paint the starting point regardless.
    {PaintedSet, PaintedSurface} =
        run_robot(Robot, Start, {white, Surface}, sets:new()),
    print(PaintedSet, PaintedSurface).

to_char(white) ->
    64;
to_char(black) ->
    32.

%% We will obtain upper and lower bounds on X and Y by analyzing
%% PaintedSet. Alternatively we could try some sparse folding trickery
%% on arrays, but that gets hairy.
print(PaintedSet, PaintedSurface) ->
    {{UpperX, LowerX}, {UpperY, LowerY}} = set_boundaries(PaintedSet),
    lists:foreach(fun(Row) ->
                          lists:foreach(fun(Col) ->
                                                {Color, _} = matrix:get_value({Col, Row}, PaintedSurface),
                                                io:format("~c", [to_char(Color)])
                                        end, lists:seq(LowerX, UpperX)),
                          io:format("~n")
                  end, lists:reverse(lists:seq(LowerY, UpperY))).

set_boundaries(Set) ->
    sets:fold(fun(Coord, Bounds) ->
                      compare_bounds(Coord, Bounds)
              end, {{0, 50000}, {0, 50000}}, Set).

%% Technically this is inadequate if the fold function is pathological
%% and/or the number of values is too small; e.g.:
%%
%% > day11:compare_bounds({5, 8}, {{0, 500}, {0, 500}}).
%% {{5,500},{8,500}}
compare_bounds({X, Y}, {XBounds, YBounds}) ->
    {compare_bounds(X, XBounds), compare_bounds(Y, YBounds)};
compare_bounds(I, {Upper, Lower}) when I > Upper ->
    {I, Lower};
compare_bounds(I, {Upper, Lower}) when I < Lower ->
    {Upper, I};
compare_bounds(_, Bounds) ->
    Bounds.

color_int(black) ->
    0;
color_int(white) ->
    1.

color_atom(0) ->
    black;
color_atom(1) ->
    white.

direction_atom(0) ->
    left;
direction_atom(1) ->
    right.

facing_atom(0) ->
    up;
facing_atom(90) ->
    right;
facing_atom(180) ->
    down;
facing_atom(270) ->
    left;
facing_atom(-90) ->
    left;
facing_atom(360) ->
    up.

facing_int(up) ->
    0;
facing_int(right) ->
    90;
facing_int(down) ->
    180;
facing_int(left) ->
    270.

advance({X, Y}, up) ->
    {X, Y+1};
advance({X, Y}, down) ->
    {X, Y-1};
advance({X, Y}, left) ->
    {X-1, Y};
advance({X, Y}, right) ->
    {X+1, Y}.

next_spot({X, Y, Facing}, left) ->
    NewFacing = facing_atom(facing_int(Facing)-90),
    {NewX, NewY} = advance({X, Y}, NewFacing),
    {NewX, NewY, NewFacing};
next_spot({X, Y, Facing}, right) ->
    NewFacing = facing_atom(facing_int(Facing)+90),
    {NewX, NewY} = advance({X, Y}, NewFacing),
    {NewX, NewY, NewFacing}.

coords({X, Y, _Facing}) ->
    {X, Y}.

run_robot(Robot, CurSpot, {CurColor, Surface}, PaintedSet) ->
    Coords = coords(CurSpot),
    NewPaintedSet = sets:add_element(Coords, PaintedSet),
    case camera_input(Robot, CurColor) of
        {terminated, NewRobot} ->
            {[PaintColor, _Direction], _NewR} =
                intcode:extract_outputs(NewRobot),
            NewSurface =
                matrix:set_value(Coords, color_atom(PaintColor), Surface),
            {NewPaintedSet, NewSurface};
        {blocked, NewRobot} ->
            {[PaintColor, Direction], NewNewRobot} =
                intcode:extract_outputs(NewRobot),
            NewSurface =
                matrix:set_value(Coords, color_atom(PaintColor), Surface),
            NextSpot = next_spot(CurSpot, direction_atom(Direction)),
            run_robot(NewNewRobot, NextSpot,
                      matrix:get_value(coords(NextSpot), NewSurface),
                      NewPaintedSet)
    end.
