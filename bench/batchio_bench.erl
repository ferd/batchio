-module(batchio_bench).
-compile(export_all).

%% Opts can be `noop' or `{passthrough, pid() | atom()}'
run(BufSize, PageSize, Opts, N, ByteSize) ->
    Config = init(BufSize, PageSize, Opts),
    Msgs = messages(N, ByteSize),
    Count = length(Msgs),
    try
        io:format("Running regular benches~n"),
        Regular = timer:tc(?MODULE, regular, [Config, Count, Msgs]),
        io:format("Running batchio benches~n"),
        Batchio = timer:tc(?MODULE, batchio, [Config, Count, Msgs]),
        {ok, [{regular, Regular},{batchio, Batchio}]}
    catch
        Type:Reason ->
            {error, {Type,Reason}, erlang:get_stacktrace()}
    after
        terminate(Config)
    end.

init(BufSize, PageSize, Opts) ->
    IoDevice = spawn(batchio_server, loop, [Opts]),
    application:load(batchio),
    application:set_env(batchio, buffer, BufSize),
    application:set_env(batchio, page_size, PageSize),
    application:set_env(batchio, leader, IoDevice),
    ok = application:start(pobox),
    ok = application:start(batchio),
    [{io,IoDevice}].

terminate([{io,IoDevice}]) ->
    exit(IoDevice, kill),
    application:stop(batchio),
    application:stop(pobox).

%% Creates `N' messages of random a-z characters, each message containing
%% `ByteSize' in terms of characters
messages(N, ByteSize) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    [[$a+random:uniform($z-$a) || _ <- lists:seq(1, ByteSize)]
     || _ <- lists:seq(1,N)].

regular(_Config, 0, []) ->
    ok;
regular(Config=[{io,IoDevice}], N, [H|T]) ->
    io:format(IoDevice, H, []),
    regular(Config, N-1, T).

batchio(_, N, Messages) ->
    _ = [batchio:format(Msg) || Msg <- Messages],
    wait_for(N).

wait_for(N) ->
    {_,Dict} = erlang:process_info(whereis(batchio_serv), dictionary),
    case lists:keyfind(total, 1, Dict) of
        {total,N} ->
            [{total,N},lists:keyfind(dropped,1,Dict)];
        _ ->
            timer:sleep(10),
            wait_for(N)
    end.
