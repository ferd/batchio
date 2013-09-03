-module(batchio_server).
-export([loop/1]).

%% We only do output requests, and no multiple-requests things
loop(Opts) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            From ! {io_reply, ReplyAs, handle(Request, Opts)},
            loop(Opts)
    end.

handle(Request, [noop]) -> noop(Request);
handle(Request, [{passthrough, Entity}]) -> passthrough(Entity, Request).

noop({put_chars, _Encoding, _Characters}) -> ok;
noop({put_chars, _Encoding, Module, Function, Args}) -> apply(Module,Function,Args), ok;
noop({put_chars, _Characters}) -> ok;
noop({put_chars, Module, Function, Args}) -> apply(Module, Function, Args), ok;
noop(_) -> {error, request}.

passthrough(Entity,Request) ->
    Ref = make_ref(),
    Entity ! {io_request, self(), Ref, Request},
    receive
        {io_reply, Ref, Reply} -> Reply
    end.
