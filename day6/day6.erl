%% Binary search tree.
-module(day6).
-compile(export_all).

build_tree(Filename) ->
    build(collect_nodes(Filename)).

collect_nodes(Filename) ->
    Fh = myio:open_file(Filename),
    fold_input(fun(Str, Dict) -> [Parent, Child] = myio:split_line(Str, ")"),
                                 dict:update(Parent, fun(Children) -> [Child|Children] end,
                                             [Child], Dict)
               end,
               dict:new(), fun() -> myio:next_line(Fh) end).

fold_input(FoldFun, Acc, InputFun) ->
    case InputFun() of
        eof ->
            Acc;
        Val ->
            fold_input(FoldFun, FoldFun(Val, Acc), InputFun)
    end.

build(Dict) ->
    Root = rosetree:root("COM"),
    expand(Root, Dict).

expand({Label, []}, Dict) ->
    case dict:find(Label, Dict) of
        error ->
            {Label, []};
        {ok, Children} ->
            {Label, lists:map(fun(C) -> expand({C, []}, Dict) end,
                              Children)}
    end.

part1(Filename) ->
    Tree = build_tree(Filename),
    rosetree:cumulative_height(Tree).

part2(Filename) ->
    Tree = build_tree(Filename),
    rosetree:distance("YOU", "SAN", Tree).
