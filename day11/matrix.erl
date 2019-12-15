-module(matrix).
-compile(export_all).

%% Each nested array is a row, the outer array is the Y axis. We're
%% creating a sparse matrix of sparse arrays.
%%
%% Every function returns a new matrix; some just happen to return
%% some other value along with it. Every retrieval may involve
%% instantiating or extending a sparse array.
initialize(DefaultValue) ->
    DefaultFun = fun() -> {r, array:new([{default, DefaultValue}])} end,
    array:new([{default, {f, DefaultFun}}]).

get_row(Y, Matrix) ->
    case array:get(Y, Matrix) of
        {f, InitFun} ->
            NewMatrix = array:set(Y, InitFun(), Matrix),
            get_row(Y, NewMatrix);
        {r, Row} ->
            {Row, Matrix}
    end.

get_value({X, Y}, Matrix) ->
    {Row, NewMatrix} = get_row(Y, Matrix),
    {array:get(X, Row), NewMatrix}.

set_value({X, Y}, Value, Matrix) ->
    {Row, NewMatrix} = get_row(Y, Matrix),
    NewRow = array:set(X, Value, Row),
    array:set(Y, {r, NewRow}, NewMatrix).
