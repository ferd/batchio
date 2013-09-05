-module(batchio_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-define(wait_msg(PAT),
    (fun() ->
        receive
            PAT -> ok
        after 2000 ->
            error({wait_too_long})
        end
    end)()).

-define(wait_msg(PAT, RET),
    (fun() ->
        receive
            PAT -> RET
        after 2000 ->
            error({wait_too_long})
        end
    end)()).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, with_leader}].

groups() ->
    [{with_leader, [], [
       not_started, sends, drops, page_boundaries
      ]}].

init_per_suite(Config) ->
    ok = application:start(pobox),
    [{buffer, 40000}, % default size of buffer
     {page_size, 4096}, % default byte size of io page
     {leader, group_leader()} % default group leader
     | Config].

end_per_suite(_Config) ->
    application:stop(pobox),
    ok.

group(_GroupName) ->
    [].

init_per_group(with_leader, Config) ->
    IoDevice = spawn(?MODULE, loop, [[{passthrough, leader_name()}]]),
    [{leader, IoDevice}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(not_started, Config) ->
    become_leader(),
    Config;
init_per_testcase(drops, Config) ->
    Buffer = 2,
    application:load(batchio),
    application:set_env(batchio, buffer, Buffer),
    application:set_env(batchio, page_size, ?config(page_size, Config)),
    application:set_env(batchio, leader, ?config(leader, Config)),
    ok = application:start(batchio),
    become_leader(),
    [{buffer, Buffer} | Config];
init_per_testcase(page_boundaries, Config) ->
    PageSize = 10, % in bytes
    application:load(batchio),
    application:set_env(batchio, buffer, ?config(buffer, Config)),
    application:set_env(batchio, page_size, PageSize),
    application:set_env(batchio, leader, ?config(leader, Config)),
    ok = application:start(batchio),
    become_leader(),
    [{page_size, PageSize} | Config];
init_per_testcase(_TestCase, Config) ->
    application:load(batchio),
    application:set_env(batchio, buffer, ?config(buffer, Config)),
    application:set_env(batchio, page_size, ?config(page_size, Config)),
    application:set_env(batchio, leader, ?config(leader, Config)),
    ok = application:start(batchio),
    become_leader(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:stop(batchio),
    remove_leader(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
not_started(Config) ->
    %% Things should be going through io:format fine when we don't
    %% have batchio started
    undefined = whereis(batchio_box),
    spawn(fun() ->
        group_leader(?config(leader, Config), self()),
        batchio:format("hello ~s~n", ["test"])
    end),
    {To,As,Req} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    To ! {io_reply, As, ok},
    {put_chars,unicode,io_lib,format,[Format,Args]} = Req,
    "hello ~s~n" = Format,
    ["test"] = Args.

sends(_Config) ->
    %% we start with a dummy request that can be used to test buffering, given
    %% we don't answer right away
    batchio:format("init"), % we are non-blocking with batchio, yay
    {To1,As1,Req1} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    {put_chars,unicode,Chars1} = Req1, % no format call needed
    "init" = unicode:characters_to_list(Chars1),
    %% Send messages to be buffered
    batchio:format("a~p ",[1]),
    batchio:format("a~p ",[2]),
    batchio:format("a~p ",[3]),
    %% Reply to the io server
    To1 ! {io_reply, As1, ok},
    %% Get the buffered messages and reply straight away
    {To2,As2,Req2} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    To2 ! {io_reply, As2, ok},
    %% Check message format
    {put_chars,unicode,Chars2} = Req2, % no format call needed
    "a1 a2 a3 " = unicode:characters_to_list(Chars2). % in order!

drops(Config) ->
    %% we start with a dummy request that can be used to test buffering, given
    %% we don't answer right away. That value is taken off the buffer
    batchio:format("init"), % we are non-blocking with batchio, yay
    {To1,As1,Req1} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    {put_chars,unicode,Chars1} = Req1, % no format call needed
    "init" = unicode:characters_to_list(Chars1),
    %% Send BufferSize+5 messages
    Max = ?config(buffer, Config),
    Msgs = Max+5,
    [batchio:format("x") || _ <- lists:seq(1, Msgs)],
    To1 ! {io_reply, As1, ok},
    %% The data is sent to us in one block, stats are updated after
    %% Get the buffered messages and reply straight away
    {To2,As2,Req2} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    To2 ! {io_reply, As2, ok},
    %% Check message format
    {put_chars,unicode,Chars2} = Req2,
    Max = length(unicode:characters_to_list(Chars2)),
    %% Check stats. We add 1 to sent and total to account for the "init" msg
    wait_until(fun() ->
        {_,Dict} = process_info(whereis(batchio_serv), dictionary),
        ct:pal("~p",[Dict]),
        Msgs+1 =:= proplists:get_value(total, Dict)
        andalso
        5 =:= proplists:get_value(dropped, Dict)
        andalso
        Max+1 =:= proplists:get_value(sent, Dict)
    end, 100, 10).

page_boundaries(Config) ->
    %% Message Size > Page Size
    PageSize = ?config(page_size, Config),
    LargeMsg = lists:duplicate(PageSize*2, $a),
    batchio:format("~s", [LargeMsg]),
    {To1,As1,Req1} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    {put_chars,unicode,Chars1} = Req1, % no format call needed
    LargeMsg = unicode:characters_to_list(Chars1),
    %% Before accepting, we prepare the next call:
    %%  two 1 byte messages get sent alone if the next one is too large for the
    %%  buffer. Then we get the big one.
    batchio:format("1"),
    batchio:format("2"),
    batchio:format(LargeMsg),
    To1 ! {io_reply, As1, ok},
    {To2,As2,Req2} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    {put_chars,unicode,Chars2} = Req2,
    "12" = unicode:characters_to_list(Chars2),
    To2 ! {io_reply, As2, ok},
    {To3,As3,Req3} = ?wait_msg({io_request, To, Ref, Req}, {To,Ref,Req}),
    To3 ! {io_reply, As3, ok},
    {put_chars,unicode,Chars3} = Req3, % no format call needed
    LargeMsg = unicode:characters_to_list(Chars3).



%%%===================================================================
%%% Helpers
%%%===================================================================

leader_name() -> test_io_leader.
become_leader() -> erlang:register(leader_name(), self()).
remove_leader() -> erlang:unregister(leader_name()).

wait_until(Fun, _, 0) -> error({timeout, Fun});
wait_until(Fun, Interval, Tries) ->
    case Fun() of
        true -> ok;
        false ->
            timer:sleep(Interval),
            wait_until(Fun, Interval, Tries-1)
    end.

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
