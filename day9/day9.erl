-module(day9).
-compile(export_all).

-include("intcode.hrl").

part1(Filename) ->
    Executable = intcode:load(Filename),
    {terminated, #state{output=[O]}} = intcode:run([1], Executable),
    O.

part2(Filename) ->
    Executable = intcode:load(Filename),
    {terminated, #state{output=[O]}} = intcode:run([2], Executable),
    O.
