-module(day11).
-compile(export_all).

-include("intcode.hrl").

camera_input(Robot, ColorAtom) ->
    intcode:execute(continue, intcode:add_input(Robot, color_int(ColorAtom))).

part1(Filename) ->
    Robot = intcode:load(Filename),
    Surface = matrix:initialize(black),
    Start = {500, 500, up}, %% X, Y, Direction
    run_robot(Robot, Start, matrix:get_value(coords(Start), Surface), sets:new()).

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
        {terminated, _State} ->
            sets:size(NewPaintedSet);
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
