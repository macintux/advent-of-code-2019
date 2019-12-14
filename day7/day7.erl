-module(day7).
-compile(export_all).

-include("intcode.hrl").

%% Stolen from Erlang's documentation on list comprehensions
permutations([]) ->
    [[]];
permutations(L)  ->
    [[H|T] || H <- L, T <- permutations(L--[H])].

%% Will no longer work with day 9's intcode module and changes to the
%% phase handling for part 2.
%% part1(Filename) ->
%%     Phases = permutations([0, 1, 2, 3, 4]),
%%     Executable = intcode:load(Filename),
%%     compare_phases(Executable, Phases, {[], -1}).

simple() ->
    Phases = [[9,8,7,6,5]],
    Amp = intcode:load("simple-p2.txt"),
    compare_phases(Phases, {[], -1}, Amp).

part2(Filename) ->
    Phases = permutations([5, 6, 7, 8, 9]),
    Amp = intcode:load(Filename),
    compare_phases(Phases, {[], -1}, Amp).

pick_highest({_P1, O1}, {_P2, O2}=Newer) when O2 > O1 ->
    Newer;
pick_highest(Older, _Newer) ->
    Older.

build_amps(Phases, Amp) ->
    build_amps(Phases, 0, array:new(), Amp).

build_amps([], _Counter, Array, _AmpTemplate) ->
    Array;
build_amps([H|T], Counter, Array, AmpTemplate) ->
    build_amps(T, Counter+1,
               array:set(Counter,
                         intcode:add_input(AmpTemplate, H),
                         Array),
               AmpTemplate).

compare_phases([], {Inputs, Max}, _Amp) ->
    Max;
compare_phases([H|T], Old, Amp) ->
    New = try_phase(build_amps(H, Amp)),
    compare_phases(T, pick_highest(Old, {H, New}), Amp).

run_next_amp(Amp, LastAmpOut) ->
    intcode:execute(continue, intcode:add_inputs(Amp, LastAmpOut)).

try_phase(Amps) ->
    try_phase(Amps, run_next_amp(array:get(0, Amps), [0]), 0).

try_phase(_Amps, {terminated, #state{output=Out}}, 4) ->
    lists:last(Out);
try_phase(Amps, {_Status, #state{output=Out}=New}, Index) ->
    NextIndex = (Index+1) rem array:size(Amps),
    {NextStatus, AmpUpdate} = run_next_amp(array:get(NextIndex, Amps),
                                           lists:reverse(Out)),
    try_phase(array:set(Index, New#state{output=[]}, Amps),
              {NextStatus, AmpUpdate}, NextIndex).
