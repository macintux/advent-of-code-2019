-module(myio).
-compile(export_all).

open_file(Filename) ->
    {ok, Fh} = file:open(Filename, [read]),
    Fh.

all_integers(Filename) ->
    fold_file(fun(X, Acc) -> [X|Acc] end, [], Filename).

fold_file(Fun, Acc, Filename) ->
    fold_file(Fun, Acc, Filename, fun grab_line/1).

fold_file(Fun, Acc, Filename, InputFun) ->
    Fh = {fh, open_file(Filename)},
    real_fold_file(Fun, Acc, InputFun(Fh), Fh, InputFun).

real_fold_file(_Fun, Acc, eof, _Fh, _InputFun) ->
    Acc;
real_fold_file(Fun, Acc, Input, Fh, InputFun) ->
    real_fold_file(Fun, lists:foldl(Fun, Acc, Input), InputFun(Fh), Fh, InputFun).

grab_records(InputFun, SplitFun, RecordFun) ->
    next_record(SplitFun(InputFun()), {InputFun, SplitFun, RecordFun}, []).

next_record(eof, {_, _, _}, Accum) ->
    lists:reverse(Accum);
next_record(Array, {I, S, R}, Accum) ->
    next_record(S(I()), {I, S, R}, [R(Array)|Accum]).

grab_line({fh, Fh}) ->
    grab_line(fun() -> next_line(Fh) end, fun split_line/1, fun erlang:list_to_integer/1).

grab_line() ->
    grab_line(fun next_line/0, fun split_line/1, fun erlang:list_to_integer/1).

grab_line(InputFun, SplitFun, ConvertFun) ->
    NextLine = SplitFun(InputFun()),
    case NextLine of
        eof ->
            eof;
        _ ->
            lists:map(ConvertFun, NextLine)
    end.

split_line(eof) ->
    eof;
split_line(Line) ->
    split_line(Line, "\t ").

split_line(Line, Delim) ->
    string:tokens(Line, Delim).

next_line() ->
    chomp(io:get_line("")).

next_line(Fh) ->
    chomp(io:get_line(Fh, "")).

chomp(eof) ->
    eof;
chomp(String) ->
    maybe_chomp(string:tokens(String, "\r\n")).

maybe_chomp([]) ->
    [];
maybe_chomp([Line | _Leftover]) ->
    Line.


%% Factor solution for further contemplation:

%% USING: io kernel math math.combinatorics math.parser prettyprint
%% sequences sequences.deep sorting splitting ;
%% IN: advent-of-code.corruption-checksum

%% : range-diff  ( seq -- n )   [ supremum ] [ infimum ] bi - ;
%% : solu1       ( seq -- )     [ range-diff ] map-sum . ;
%% : div-order   ( seq -- seq ) natural-sort reverse ;
%% : divisible   ( seq -- seq ) [ div-order first2 mod 0 = ] filter ;
%% : quotient    ( seq -- n )   divisible flatten div-order first2 / ;
%% : solu2       ( seq -- )     [ 2 <combinations> quotient ] map-sum . ;
%% : strseq>num  ( seq -- seq ) [ string>number ] map ;
%% : parse-input ( -- seq )     lines [ "\t" split strseq>num ] map ;
%% : main        ( -- )         parse-input [ solu1 ] [ solu2 ] bi ;

%% MAIN: main
