-module(day8).
-compile(export_all).


%% Logic for part 2:
%%   Read Width*Height blocks of integers from the file
%%   Superimpose them per the rules for day 8
%%   Split the blocks into rows (lines)
%%   Print each line
part2(Filename, Width, Height) ->
    Fh = myio:open_file(Filename),
    OuterFun = fun() ->
                       consolidate(fun() ->
                                           layers:read_block(Fh, Width*Height)
                                   end)
               end,
    print_until_end(OuterFun(), OuterFun, Width).

print_until_end(eof, _Fun, _Width) ->
    done;
print_until_end(ImageBlock, Fun, Width) ->
    render(layers:block_to_rows(ImageBlock, Width, [])),
    print_until_end(Fun(), Fun, Width).

to_string(Ints) ->
    lists:map(fun(1) -> 64; %% Ampersand
                 (0) -> 32  %% Blank
              end, Ints).


render([]) ->
    done;
render([H|T]) ->
    io:format("~s~n", [to_string(H)]),
    render(T).


consolidate(Fun) ->
    consolidate(Fun(), Fun, []).

consolidate(eof, _Fun, []) ->
    eof;
consolidate(eof, _Fun, Accum) ->
    Accum;
consolidate(Image, Fun, Accum) ->
    consolidate(Fun(), Fun, apply_image_mask(Image, Accum, [])).

apply_image_mask([], [], New) ->
    lists:reverse(New);
apply_image_mask(FirstLayer, [], []) ->
    FirstLayer;
apply_image_mask([_H1|T1], [H2|T2], Accum) when H2 /= 2 ->
    apply_image_mask(T1, T2, [H2|Accum]);
apply_image_mask([H|T1], [2|T2], Accum) ->
    apply_image_mask(T1, T2, [H|Accum]).




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
