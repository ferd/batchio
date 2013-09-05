-module(batchio_serv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {box, leader}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, BufferSize} = application:get_env(batchio, buffer),
    {ok, Pid} = pobox:start_link({local,batchio_box}, self(), BufferSize, queue, notify),
    Leader = case application:get_env(batchio, leader) of
        undefined -> group_leader();
        {ok, undefined} -> group_leader();
        {ok, Term} -> Term
    end,
    put(total, 0),
    put(sent, 0),
    put(dropped, 0),
    set_active(Pid),
    {ok, #state{box=Pid, leader=Leader}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({mail,Box,Msgs,Count,Drops}, S=#state{box=Box, leader=Leader}) ->
    io:put_chars(Leader, Msgs),
    put(total, get(total)+Count+Drops),
    put(sent, get(sent)+Count),
    put(dropped, get(dropped)+Drops),
    set_active(Box),
    {noreply, S};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
set_active(Box) ->
    {ok, PageSize} = application:get_env(batchio, page_size), % bytes
    pobox:active(Box, fun accumulator/2, {0, PageSize}).

accumulator(Text, {Current, Max}) ->
    case iolist_size(Text)+Current of
        N when N > Max ->
            %% it's possible the entry is > than the buffer size. When that
            %% happens, we let it through on its own to avoid blocking entirely
            case Current of
                0 -> {{ok, Text}, {N, Max}};
                _ -> skip
            end;
        N ->
            {{ok, Text}, {N,Max}}
    end.
