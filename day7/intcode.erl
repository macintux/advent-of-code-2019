-module(intcode).
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
%%
%% User inputs are now supplied via a second argument to the operator
%% function, so the arguments are: a list of parameters, a 2-tuple with
%% inputs and outputs, and the array representing current memory.
%%
%% The outcome is a 3-tuple: `memory`, `new_ip`, or `end_of_execution`
%% indicating the outcome, the IO data, and the new memory.
%%
%% If a new instruction pointer is generated (`new_ip`) the last
%% element of the 3-tuple will itself be a tuple with the new
%% instruction pointer and the new memory
operators() ->
    #{
      %% 1: add
      1 => {
            "add",
            3,
            fun([MR1, MR2, {position, M3}], IO, Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    {memory, IO, array:set(M3, M1+M2, Memory)}
            end
           },

      %% 2: mult values stored at parameters 1 and parameter 2,
      %%    results go to address in parameter 3
      2 => {
            "mult",
            3,
            fun([MR1, MR2, {position, M3}], IO, Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    {memory, IO, array:set(M3, M1*M2, Memory)}
            end},

      3 => {
            "input",
            1,
            fun([{position, M1}], {[], Out}, Memory) ->
                    {ok, [Input]} = io:fread("Op3 input? ", "~d"),
                    {memory, {[], Out}, array:set(M1, Input, Memory)};
               ([{position, M1}], {[Input|T], Out}, Memory) ->
                    ?DEBUG("Used ~p as Op3 input~n", [Input]),
                    {memory, {T, Out}, array:set(M1, Input, Memory)}
            end
           },

      4 => {
            "output",
            1,
            fun([MR1], {In, Out}, Memory) ->
                    M1 = param(MR1, Memory),
                    ?DEBUG("Op4 output: ~B~n", [M1]),
                    {memory, {In, [M1|Out]}, Memory}
            end
           },

      5 => {
            "jump-if-true",
            2,
            fun([MR1, MR2], IO, Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 /= 0 of
                        true ->
                            {new_ip, IO, {M2, Memory}};
                        false ->
                            {memory, IO, Memory}
                    end
            end
           },

      6 => {
            "jump-if-false",
            2,
            fun([MR1, MR2], IO, Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 == 0 of
                        true ->
                            {new_ip, IO, {M2, Memory}};
                        false ->
                            {memory, IO, Memory}
                    end
            end
           },

      7 => {
            "less-than",
            3,
            fun([MR1, MR2, {position, M3}], IO, Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 < M2 of
                        true ->
                            {memory, IO, array:set(M3, 1, Memory)};
                        false ->
                            {memory, IO, array:set(M3, 0, Memory)}
                    end
            end
           },

      8 => {
            "equals",
            3,
            fun([MR1, MR2, {position, M3}], IO, Memory) ->
                    [M1, M2] = params([MR1, MR2], Memory),
                    case M1 == M2 of
                        true ->
                            {memory, IO, array:set(M3, 1, Memory)};
                        false ->
                            {memory, IO, array:set(M3, 0, Memory)}
                    end
            end
           },


      99 => {
             "terminate",
             0,
             fun([], IO, Memory) -> {end_of_execution, IO, Memory} end
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


execute(IP, Memory, IO, Operators) ->
    {Op, Modes} = decipher_operator(array:get(IP, Memory)),
    #{Op := {Label, ParamCount, OpFun}} = Operators,
    Parameters = map_by_mode(retrieve(IP+1, ParamCount, Memory), Modes, []),
    ?DEBUG("Executing ~s~n", [Label]),
    check_execution(IP+ParamCount+1,
                    OpFun(Parameters, IO, Memory),
                    Operators).

map_by_mode([], _Modes, Acc) ->
    lists:reverse(Acc);
map_by_mode([H|T], [], Acc) ->
    map_by_mode(T, [], [{position, H}|Acc]);
map_by_mode([H|T], [M|Rest], Acc) ->
    map_by_mode(T, Rest, [{M, H}|Acc]).

check_execution(_NextIP, {end_of_execution, IO, NewMem}, _Ops) ->
    {IO, NewMem};
check_execution(_NextIP, {new_ip, IO, {RealNextIP, NewMem}}, Ops) ->
    execute(RealNextIP, NewMem, IO, Ops);
check_execution(NextIP, {memory, IO, NewMem}, Ops) ->
    execute(NextIP, NewMem, IO, Ops).


run(Memory) ->
    run(Memory, []).

run(Memory, Inputs) ->
    {{_Inputs, Outputs}, _NewMem} = execute(0, Memory, {Inputs, []}, operators()),
    Outputs.

load(Filename) ->
    array:from_list(myio:all_integers(Filename)).
