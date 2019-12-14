-module(layers).
-compile(export_all).

read_block(Fh, Count) ->
    split_ints(file:read(Fh, Count)).

split_ints(eof) ->
    eof;
split_ints({ok, Layer}) when length(Layer) < 3 ->
    %% newline at end of file
    eof;
split_ints({ok, Layer}) ->
    lists:map(fun(C) -> C - $0 end,
              Layer).

block_to_rows([], _Width, Accum) ->
    lists:reverse(Accum);
block_to_rows(Bytes, Width, Accum) ->
    {Row, Rest} = lists:split(Width, Bytes),
    block_to_rows(Rest, Width, [Row|Accum]).
