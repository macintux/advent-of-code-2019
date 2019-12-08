-module(day8).
-compile(export_all).

part1(Filename, Width, Height) ->
    {ok, Fh} = file:open(Filename, [read]),
    [_Zeroes, Ones, Twos] = pick_layer_fewest_zeroes(
                              fun() -> file:read(Fh, Width*Height) end
                             ),
    Ones * Twos.

pick_layer_fewest_zeroes(Fun) ->
    pick_layer_fewest_zeroes(Fun(), Fun, [1000, -1, -1]).

pick_layer_fewest_zeroes(eof, _Fun, Tallies) ->
    Tallies;
pick_layer_fewest_zeroes({ok, Layer}, Fun, Tallies) when length(Layer) < 3 ->
    Tallies;
pick_layer_fewest_zeroes({ok, Layer}, Fun, Tallies) ->
    pick_layer_fewest_zeroes(Fun(), Fun,
                             maybe_update_tallies(count_layer(Layer), Tallies)).

maybe_update_tallies([NZ, NO, NT], [OZ, _OO, _OT]) when NZ < OZ ->
    [NZ, NO, NT];
maybe_update_tallies(_New, Old) ->
    Old.

count_layer(Layer) ->
    count_layer(myio:chomp(Layer), [0, 0, 0]).

count_layer([], Tallies) ->
    Tallies;
count_layer([$0|R], [Z, O, T]) ->
    count_layer(R, [Z+1, O, T]);
count_layer([$1|R], [Z, O, T]) ->
    count_layer(R, [Z, O+1, T]);
count_layer([$2|R], [Z, O, T]) ->
    count_layer(R, [Z, O, T+1]).
