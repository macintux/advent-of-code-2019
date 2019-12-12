-module(ic_ops).
-compile(export_all).

-include("intcode.hrl").

%% Functions that the intcode operators can use

state_memory(#state{memory=M}) ->
    M.

state_input(#state{input=I}) ->
    I.

state_base(#state{base=B}) ->
    B.

state_output(#state{output=O}) ->
    O.

state_operators(#state{operators=O}) ->
    O.

find_memory(Addr, Memory) ->
    {array:get(Addr, Memory), Memory}.

update_memory(Addr, Val, Memory) ->
    array:set(Addr, Val, Memory).

param_value({immediate, Param}, _Base, _Memory) ->
    {Param, undefined};
param_value({position, Param}, _Base, Memory) ->
    find_memory(Param, Memory);
param_value({relative, Param}, Base, Memory) ->
    find_memory(Base + Param, Memory).

params_as_value(Params, #state{memory=Mem, base=Base}) ->
    lists:map(fun(P) -> param_value(P, Base, Mem) end,
              Params).

param_as_address({position, P}, #state{memory=Mem}) ->
    {P, Mem};
param_as_address({relative, R}, #state{memory=Mem, base=Base}) ->
    {Base+R, Mem}.
