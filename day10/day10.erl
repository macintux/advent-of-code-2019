-module(day10).
-compile(export_all).

%% See https://adventofcode.com/2019/day/10 for one of the more
%% fascinating problems I've had to solve thus far.
%%
%% Based on a hint I saw in Reddit, I chose to use polar coordinates,
%% which made everything much easier, particularly once I realized the
%% trick to part 2...
%%
%% Here's the secret to part 2: if the top left-hand corner of the
%% input is considered {0, 0}, the simplest way to calculate and
%% manipulate polar coordinates is to rotate the asteroid field 90º
%% clockwise.
%%
%% 0,0 becomes the *lower* left-hand corner, and now "up" (to the
%% right) is positive, and the new right (the old "down") is also
%% positive.
%%
%% This means that cell/2 will switch row and column (Y and X).
%%
%% This also has the benefit during part 2 that the old "up", now the
%% left, is the both the start of sorting the angles from +π to -π,
%% and where we want to start lasering asteroids out of existence.

%% Character 10 is the UNIX end of line
cell(10, {R, _C, A, E}) ->
    {R+1, 0, A, E};
%% 46: Period (empty space in the asteroid field)
cell(46, {R, C, A, E}) ->
    {R, C+1, A, [{R,C}|E]};
%% 35: Hash tag for an asteroid
cell(35, {R, C, A, E}) ->
    {R, C+1, [{R,C}|A], E}.

%% Take the input file and convert to a list of asteroids (in
%% Cartesian coordinates, rotated 90º clockwise per the notes at the
%% top of this file) and a list of empty spaces. We never actually use
%% the empty spaces but including them is cheap.
initialize(Filename) ->
    {_Rows, _Cols, Asteroids, Empty} =
        myio:fold_file(fun cell/2, {0, 0, [], []},
                       Filename, fun myio:next_line/1),
    {Asteroids, Empty}.

part1(Filename) ->
    {Asteroids, _Empty} = initialize(Filename),
    {{CX, CY}, Hidden, _PCoords} = find_best_start(Asteroids, Asteroids),

    %% At the end we calculate the visible tally based on how many
    %% occluded asteroids we (didn't) see. It's more direct to just
    %% count the number of theta angles we have in our coordinates.
    %% io:format("Visible: ~p~n", [visible(PCoords)]),

    %% The candidate coordinates are reversed to orient our asteroid
    %% field correctly for part 2, hence {CY, CX}.
    io:format("Location: ~p~nHidden: ~p~nVisible: ~p~n",
              [{CY, CX}, Hidden, length(Asteroids) - Hidden - 1]).

part2(Filename) ->
    {Asteroids, _Empty} = initialize(Filename),
    {{OX, OY}=_Laser, _Hidden, PCoords} = find_best_start(Asteroids, Asteroids),
    {TX,TY} = polar:p2c(lists:nth(200, polar:laser_sort(PCoords))),
    %% Now that we have our origin (with rows and columns reversed)
    %% and 200th asteroid (target) we need to add those values while
    %% restoring the input's orientation.
    {OY+TY, OX+TX}.

visible(PolarCoords) ->
    polar:visible(PolarCoords).

hidden_count(PolarCoords) ->
    length(polar:occluded(PolarCoords)).

%% For each candidate asteroid, we want to recalculate every
%% asteroid's position in the field. We have them stored as Cartesian
%% coordinates, which we will adjust to account for the candidate's
%% position before converting to polar coordinates.
%%
%% In other words, if the candidate we're evaluating for suitability
%% is at {3, 4}, another asteroid at {3, 7} would have a polar
%% coordinate *relative to {3, 4}* as {3, π/2}.
polar_coords(Candidate, Asteroids) ->
    lists:map(fun polar:c2p/1,
              lists:map(fun(A) -> relocate(A, Candidate) end,
                        Asteroids)).

%% Adjust a Cartesian coordinate based on a revised origin.
relocate({X, Y}, {OX, OY}) ->
    {X-OX, Y-OY}.

find_best_start([H|T], Asteroids) ->
    PCoords = polar_coords(H, Asteroids),
    Tally = hidden_count(PCoords),
    find_best_start(T, Asteroids, {H, Tally, PCoords}).

find_best_start([], _Asteroids, Best) ->
    Best;
find_best_start([H|T], Asteroids, {OldCoord, Tally, OldPCoords}) ->
    PCoords = polar_coords(H, Asteroids),
    NewTally = hidden_count(PCoords),
    case NewTally < Tally of
        true ->
            find_best_start(T, Asteroids, {H, NewTally, PCoords});
        false ->
            find_best_start(T, Asteroids, {OldCoord, Tally, OldPCoords})
    end.
