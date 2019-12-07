-module(day2).
-compile(export_all).

registers(Idx, Array) ->
    {array:get(Idx+1, Array),
     array:get(Idx+2, Array),
     array:get(Idx+3, Array)}.

execute(Idx, Array) ->
    execute(array:get(Idx, Array), Idx, Array).

execute(99, _Idx, Array) ->
    Array;
execute(1, Idx, Array) ->
    {RI1, RI2, ROut} = registers(Idx, Array),
    NextIdx = Idx+4,
    NewProgram = array:set(ROut,
                           array:get(RI1, Array) +
                               array:get(RI2, Array),
                           Array),
    execute(array:get(NextIdx, NewProgram), NextIdx, NewProgram);
execute(2, Idx, Array) ->
    {RI1, RI2, ROut} = registers(Idx, Array),
    NextIdx = Idx+4,
    NewProgram = array:set(ROut,
                           array:get(RI1, Array) *
                               array:get(RI2, Array),
                           Array),
    execute(array:get(NextIdx, NewProgram), NextIdx, NewProgram).


part1(Filename) ->
    Program = array:from_list(myio:all_integers(Filename)),
    BuggyProgram =
        array:set(1, 12,
                  array:set(2, 2, Program)),
    Result = execute(0, BuggyProgram),
    array:get(0, Result).

%% Through trial and error:
%% > day2:part2(66, 35, "input.txt").
%% 0
%%
%% So the correct answer is 6635
part2(Noun, Verb, Filename) ->
    Program = array:from_list(myio:all_integers(Filename)),
    BuggyProgram =
        array:set(1, Noun,
                  array:set(2, Verb, Program)),
    Result = execute(0, BuggyProgram),
    19690720 - array:get(0, Result).
