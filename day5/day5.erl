-module(day5).
-compile(export_all).

-define(DEBUG(X, Y), ok).
%% -define(DEBUG(X, Y), io:format(X, Y)).

params(Params, Memory) ->
    lists:map(fun(P) -> param(P, Memory) end,
              Params).

param({immediate, Param}, _Memory) ->
    Param;
param({position, Param}, Memory) ->
    array:get(Param, Memory).

%% Each operator is a 3-tuple: operation name, arity, and function to
%% be invoked with the required number of parameters.
%%
%% Any operator functions that require the last parameter to be
%% positional (so the result can be stored there) should validate that
%% `position` is the mode tag.
operators() ->
    #{
      %% 1: add
      1 => {
            "add",
            3,
            fun([MR1, MR2, {position, M3}], Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    {memory, array:set(M3, M1+M2, Memory)}
            end
           },

      %% 2: mult values stored at parameters 1 and parameter 2,
      %%    results go to address in parameter 3
      2 => {
            "mult",
            3,
            fun([MR1, MR2, {position, M3}], Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    {memory, array:set(M3, M1*M2, Memory)}
            end},

      3 => {
            "input",
            1,
            fun([{position, M1}], Memory) ->
                    {ok, [Input]} = io:fread("Op3 input? ", "~d"),
                    {memory, array:set(M1, Input, Memory)}
            end
           },

      4 => {
            "output",
            1,
            fun([MR1], Memory) ->
                    M1 = param(MR1, Memory),
                    io:format("Op4 output: ~B~n", [M1]),
                    {memory, Memory}
            end
           },

      5 => {
            "jump-if-true",
            2,
            fun([MR1, MR2], Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 /= 0 of
                        true ->
                            {new_ip, M2, Memory};
                        false ->
                            {memory, Memory}
                    end
            end
           },

      6 => {
            "jump-if-false",
            2,
            fun([MR1, MR2], Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 == 0 of
                        true ->
                            {new_ip, M2, Memory};
                        false ->
                            {memory, Memory}
                    end
            end
           },

      7 => {
            "less-than",
            3,
            fun([MR1, MR2, {position, M3}], Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 < M2 of
                        true ->
                            {memory, array:set(M3, 1, Memory)};
                        false ->
                            {memory, array:set(M3, 0, Memory)}
                    end
            end
           },

      8 => {
            "equals",
            3,
            fun([MR1, MR2, {position, M3}], Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 == M2 of
                        true ->
                            {memory, array:set(M3, 1, Memory)};
                        false ->
                            {memory, array:set(M3, 0, Memory)}
                    end
            end
           },


      99 => {
             "terminate",
             0,
             fun([], Memory) -> {end_of_execution, Memory} end
            }
     }.

retrieve(Idx, Len, Array) ->
    lists:map(fun(X) -> array:get(X, Array) end,
              lists:seq(Idx, Idx+Len-1)).

decipher_operator(Data) ->
    Op = Data rem 100,
    ParameterModes = intsplit(Data div 100, []),
    {Op,
     lists:reverse(lists:map(fun(0) -> position;
                                (1) -> immediate
                             end, ParameterModes))}.

intsplit(Val, Accum) when Val =< 0 ->
    Accum;
intsplit(Val, Accum) ->
    intsplit(Val div 10, [Val rem 10|Accum]).


execute(IP, Memory, Operators) ->
    {Op, Modes} = decipher_operator(array:get(IP, Memory)),
    #{Op := {Label, ParamCount, OpFun}} = Operators,
    Parameters = map_by_mode(retrieve(IP+1, ParamCount, Memory), Modes, []),
    ?DEBUG("Executing ~s~n", [Label]),
    check_execution(IP+ParamCount+1,
                    OpFun(Parameters, Memory),
                    Operators).

map_by_mode([], _Modes, Acc) ->
    lists:reverse(Acc);
map_by_mode([H|T], [], Acc) ->
    map_by_mode(T, [], [{position, H}|Acc]);
map_by_mode([H|T], [M|Rest], Acc) ->
    map_by_mode(T, Rest, [{M, H}|Acc]).

check_execution(_NextIP, {end_of_execution, NewMem}, _Operators) ->
    NewMem;
check_execution(_NextIP, {new_ip, RealNextIP, NewMem}, Operators) ->
    execute(RealNextIP, NewMem, Operators);
check_execution(NextIP, {memory, NewMem}, Operators) ->
    execute(NextIP, NewMem, Operators).


run(Memory) ->
    _NewMem = execute(0, array:from_list(Memory), operators()),
    ok.
    %% Spare someone running this from the Erlang shell from seeing a
    %% large array as the end result.

part1(Filename) ->
    run(myio:all_integers(Filename)).

part2(Filename) ->
    %% Branching is controlled by the requested input. Part 1 needs a
    %% 1 as input, part 2 needs 5.
    part1(Filename).
