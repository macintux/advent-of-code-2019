-module(day7).
-compile(export_all).

%% Stolen from Erlang's documentation on list comprehensions
permutations([]) ->
    [[]];
permutations(L)  ->
    [[H|T] || H <- L, T <- permutations(L--[H])].

part1(Filename) ->
    Phases = permutations([0, 1, 2, 3, 4]),
    Executable = intcode:load(Filename),
    compare_phases(Executable, Phases, {[], -1}).

compare_phases(_Executable, [], {Inputs, Max}) ->
    io:format("Max found at ~p~n", [Inputs]),
    Max;
compare_phases(Executable, [H|T], Old) ->
    New = try_phase(Executable, H, Old),
    compare_phases(Executable, T, New).

try_phase(Executable, Inputs, Top) ->
    try_phase(Executable, Inputs, [0], Inputs, Top).

try_phase(_Executable, [], _Outputs, _FullInputs, Max) ->
    Max;
try_phase(Executable, [In|T], [PrevOut], FullInputs, {TopI, TopMax}) ->
    [Out] = intcode:run(Executable, [In, PrevOut]),
    case Out > TopMax of
        true ->
            try_phase(Executable, T, [Out], FullInputs, {FullInputs, Out});
        false ->
            try_phase(Executable, T, [Out], FullInputs, {TopI, TopMax})
    end.
