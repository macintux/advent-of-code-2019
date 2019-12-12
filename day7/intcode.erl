-module(intcode).
-compile(export_all).

-include("intcode.hrl").

%% Each operator is a 3-tuple: operation name, arity, and function to
%% be invoked with the required number of parameters.
%%
%% The full state is supplied as an argument, including any pending
%% inputs and queued outputs, so the two arguments to the operator
%% function are a list of parameters and the state.
%%
%% The result is a list of 2-tuples:
%%    * An atom indicating what changed:
%%      * `memory`        - memory changed
%%      * `ip`            - non-linear change to the instruction pointer
%%      * `base`          - relative base changed
%%      * `input`         - change in the input FIFO queue
%%      * `output`        - change in the output FIFO queue
%%      * `block`         - need more input
%%      * `none`          - no changes (e.g., jump that didn't)
%%      * `terminate`     - end of execution
%%    * The changed value (`undefined` if no change to state)
%%
%% Each operator function should treat the state and its components as
%% opaque, although they are expected to pattern match the parameters.
%%
%% They may only invoke functions defined in `ic_ops`.
operators() ->
    #{
      %% 1: add
      1 => {
            "add",
            3,
            fun([I1, I2, O], State) ->
                    [{V1, _}, {V2, _}] = ic_ops:params_as_value([I1, I2], State),
                    {OAddr, Block} = ic_ops:param_as_address(O, State),
                    [{memory, ic_ops:update_memory(OAddr, V1+V2, Block)}]
            end
           },

      %% 2: mult values stored at parameters 1 and parameter 2,
      %%    results go to address in parameter 3
      2 => {
            "mult",
            3,
            fun([I1, I2, O], State) ->
                    [{V1, _}, {V2, _}] = ic_ops:params_as_value([I1, I2], State),
                    {OAddr, Block} = ic_ops:param_as_address(O, State),
                    [{memory, ic_ops:update_memory(OAddr, V1*V2, Block)}]
            end
           },

      3 => {
            "input",
            1,
            fun([O], State) ->
                    case ic_ops:state_input(State) of
                        [] ->
                            [{block, undefined}];
                        [Next|T] ->
                            ?DEBUG("Used ~p as Op3 input~n", [Next]),
                            {OAddr, Block} = ic_ops:param_as_address(O, State),
                            [{memory, ic_ops:update_memory(OAddr, Next, Block)},
                             {input, T}]
                    end
            end
           },

      4 => {
            "output",
            1,
            fun([MR1], State) ->
                    [{M1, _}] = ic_ops:params_as_value([MR1], State),
                    ?DEBUG("Op4 output: ~B~n", [M1]),
                    Output = ic_ops:state_output(State),
                    [{output, Output ++ [M1]}]
            end
           },

      5 => {
            "jump-if-true",
            2,
            fun([MR1, MR2], State) ->
                    [{M1, _}, {M2, _}] =
                        ic_ops:params_as_value([MR1, MR2], State),
                    case M1 /= 0 of
                        true ->
                            [{ip, M2}];
                        false ->
                            [{none, undefined}]
                    end
            end
           },

      6 => {
            "jump-if-false",
            2,
            fun([MR1, MR2], State) ->
                    [{M1, _}, {M2, _}] =
                        ic_ops:params_as_value([MR1, MR2], State),
                    case M1 == 0 of
                        true ->
                            [{ip, M2}];
                        false ->
                            [{none, undefined}]
                    end
            end
           },

      7 => {
            "less-than",
            3,
            fun([MR1, MR2, O], State) ->
                    [{M1, _}, {M2, _}] =
                        ic_ops:params_as_value([MR1, MR2], State),
                    {OAddr, Mem} = ic_ops:param_as_address(O, State),
                    case M1 < M2 of
                        true ->
                            [{memory, ic_ops:update_memory(OAddr, 1, Mem)}];
                        false ->
                            [{memory, ic_ops:update_memory(OAddr, 0, Mem)}]
                    end
            end
           },

      8 => {
            "equals",
            3,
            fun([MR1, MR2, O], State) ->
                    [{M1, _}, {M2, _}] =
                        ic_ops:params_as_value([MR1, MR2], State),
                    {OAddr, Mem} = ic_ops:param_as_address(O, State),
                    case M1 == M2 of
                        true ->
                            [{memory, ic_ops:update_memory(OAddr, 1, Mem)}];
                        false ->
                            [{memory, ic_ops:update_memory(OAddr, 0, Mem)}]
                    end
            end
           },

      9 => {
            "base",
            1,
            fun([MR1], State) ->
                    [{M1, _}] = ic_ops:params_as_value([MR1], State),
                    OldBase = ic_ops:state_base(State),
                    [{base, OldBase+M1}]
            end
           },
      99 => {
             "terminate",
             0,
             fun([], _State) -> [{terminate, undefined}] end
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
                                (1) -> immediate;
                                (2) -> relative
                             end, ParameterModes))}.

intsplit(Val, Accum) when Val =< 0 ->
    Accum;
intsplit(Val, Accum) ->
    intsplit(Val div 10, [Val rem 10|Accum]).


%% First argument: instruction pointer
%%
%% Second argument: `continue` or `step` atom, indicating whether this
%% is being executed one instruction at a time or whether it should
%% continue until termination or blocked on input
%%
%% Return value: {Status, NextIP, State} where Status is one of these atoms:
%%    * `terminated`
%%    * `blocked`
%%    * `step`  - only possible in step mode
execute(IP, DoNext,
        #state{memory=Memory,
               operators=Operators}=State) ->
    {Op, Modes} = decipher_operator(array:get(IP, Memory)),
    ?DEBUG("Operator ~p~n", [Op]),
    #{Op := {Label, ParamCount, OpFun}} = Operators,
    Parameters = map_by_mode(retrieve(IP+1, ParamCount, Memory), Modes, []),
    ?DEBUG("Executing ~s~n", [Label]),
    check_execution(DoNext, IP+ParamCount+1,
                    OpFun(Parameters, State),
                    State).


map_by_mode([], _Modes, Acc) ->
    lists:reverse(Acc);
map_by_mode([H|T], [], Acc) ->
    map_by_mode(T, [], [{position, H}|Acc]);
map_by_mode([H|T], [M|Rest], Acc) ->
    map_by_mode(T, Rest, [{M, H}|Acc]).

%% Make sure that if `terminate` or `blocked` is one of the outcomes
%% of our operation, that's the last change in the list.
change_comp({terminate, undefined}, _) ->
    false;
change_comp({block, undefined}, _) ->
    false;
change_comp(_, _) ->
    true.

check_execution(DoNext, NextIP, Changes, State) ->
    {Status, NewNextIP, NewState} =
        apply_changes(NextIP,
                      lists:sort(fun change_comp/2, Changes),
                      State, step),
    pause_or_continue(DoNext, Status, NewNextIP, NewState).

pause_or_continue(continue, step, IP, State) ->
    execute(IP, continue, State);
pause_or_continue(_Mode, Status, IP, State) ->
    {Status, IP, State}.

%% lists:foldl would be a viable alternative to recursion here but
%% recursion seems a bit friendlier. Make sure any halting result is
%% sorted to be the last outcome.
apply_changes(IP, [], State, Next) ->
    {Next, IP, State};
apply_changes(IP, [{terminate, undefined}], State, _Next) ->
    {terminated, IP, State};
apply_changes(IP, [{block, undefined}], State, _Next) ->
    {blocked, IP, State};
apply_changes(IP, [{none, undefined}|T], State, Next) ->
    apply_changes(IP, T, State, Next);
apply_changes(_IP, [{ip, New}|T], State, Next) ->
    apply_changes(New, T, State, Next);
apply_changes(IP, [{memory, Value}|T], State, Next) ->
    apply_changes(IP, T, State#state{memory=Value}, Next);
apply_changes(IP, [{base, Value}|T], State, Next) ->
    apply_changes(IP, T, State#state{base=Value}, Next);
apply_changes(IP, [{input, Value}|T], State, Next) ->
    apply_changes(IP, T, State#state{input=Value}, Next);
apply_changes(IP, [{output, Value}|T], State, Next) ->
    apply_changes(IP, T, State#state{output=Value}, Next).

run(State) ->
    run([], State).

run(Inputs, State) ->
    execute(0, continue, State#state{input=Inputs}).

load_list(Ints) ->
    #state{memory=array:from_list(Ints, 0),
           operators=operators(),
           base=0,
           input=[],
           output=[]}.

load(Filename) ->
    load_list(myio:all_integers(Filename)).
