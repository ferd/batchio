-module(batchio).
-export([format/1, format/2]).

format(Str) -> format(Str,[]).

format(Str, Args) ->
    case whereis(batchio_box) of
        undefined ->
            io:format(Str,Args);
        Pid when is_pid(Pid) ->
            pobox:post(Pid, io_lib:format(Str,Args))
    end.
