-module(ic_mem).
-compile(export_all).

-include("intcode.hrl").

%% would go in intcode.hrl
-define(BLOCK_SIZE, 1024).


%% I would like to create blocks of memory for the intcode computer
%% but I'm going to go with a simpler array-based mechanism for
%% now. Retaining this code for future use.

%% Returns both the requested value and the memory block from which it
%% was obtained as a 2-tuple.
find_memory(Address, []) ->
    build_mem([], %% XXX have to pick up here too, construct new block, need function to map address to block boundary (and then use that in other parts of this file)
    throw({address_not_found, Address});
find_memory(Address, [#mem{start=S,contents=C}=M|T])
  when S =< Address and S+?BLOCK_SIZE > Address ->
    {array:get(Address-S, C), M};
find_memory(Address, [_M|T]) ->
    find_memory(Address, T).


update_memory(Addr, _Val, #mem{start=S}) when S > Addr or S+?BLOCK_SIZE =< Addr ->
    throw(address_out_of_range);
update_memory(Address, Val, #mem{start=S,contents=C}=M) ->
    M#mem{array:set(Address-S, Val, C)}.

build_mem(Integers) ->
    build_mem(Integers, 0).

build_mem(Integers, Offset) ->
    build_mem(Integers, Offset, ?BLOCK_SIZE, []).

build_mem(Integers, Offset, Max, Accum) when length(Integers) > Max ->
    [First, Rest] = lists:split(Integers, Max),
    build_mem(Rest, Offset+Max, Max,
              [ #mem{start=Offset, contents=build_block(First, Max)}
                | Accum]);
build_mem(Integers, Offset, Max, Accum) ->
    lists:reverse([ #mem{start=Offset, contents=build_block(Integers, Max)}
                    | Accum]).

append(Memory, NewMem) when is_list(NewMem) ->
    sort(Memory ++ NewMem);
append(Memory, NewMem) ->
    sort([NewMem|Memory]).

sort(Memories) ->
    lists:sort(fun(#mem{start=S1}, #mem{start=S2}) ->
                       S1 < S2
               end, Memories).

build_block(Integers, Len) ->
    array:resize(Len, array:fix(array:from_list(Integers, 0))).
