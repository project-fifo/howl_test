-module(howl_test_ws_handler).

-behaviour(websocket_client_handler).


-export([
         connect/1,
         auth/3,
         join/2,
         leave/2,
         messages/1,
         close/1
        ]).

-export([
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).


-record(state, {node,
                messages = []}).

connect(Node) ->
    websocket_client:start_link(binary_to_list(iolist_to_binary(rt_howl:url(Node))), %this is stupid!
                                ?MODULE, [Node]).


auth(Pid, User, Pass) ->
    Pid ! {auth, User, Pass}.

join(Pid, Channel) ->
    Pid ! {join, Channel}.

leave(Pid, Channel) ->
    Pid ! {leave, Channel}.

close(Pid) ->
    Pid ! close.

messages(Pid) ->
    Ref = erlang:make_ref(),
    Pid ! {messages, self(), Ref},
    receive
        {messages, Ref, Ms} ->
            {ok, Ms}
    after
        5000 ->
            {error, timeout}
    end.

init([Node], _ConnState) ->
    {ok, #state{node = Node}}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};

websocket_handle({text, Msg}, _ConnState, State) ->
    JSON = jsx:decode(Msg),
    lager:debug("[~p] -> ~s~n", [State#state.node, Msg]),
    {ok, State#state{messages = [JSON | State#state.messages]}}.

websocket_info({auth, User, Pass}, _ConnState, State) ->
    reply_json([{<<"auth">>,
                 [{<<"user">>, User},
                  {<<"pass">>, Pass}]}], State);


websocket_info({messages, Pid, Ref}, _ConnState, State) ->
    Pid ! {messages, Ref, State#state.messages},
    {ok, State};

websocket_info({join, Channel}, _ConnState, State) ->
    reply_json([{<<"join">>, Channel}], State);

websocket_info({leave, Channel}, _ConnState, State) ->
    reply_json([{<<"leave">>, Channel}], State);

websocket_info(close, _ConnState, State) ->
    {stop, normal, State};

websocket_info(start, _ConnState, State) ->
    {ok, State}.


websocket_terminate({remote,closed}, _ConnState, _State) ->
    lager:debug("Websocket closed from remote end."),
    ok;

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
    lager:debug("Websocket closed in state ~p wih code ~p and payload ~p~n",
                [State, Code, Payload]),
    ok.

reply_json(JSON, State) ->
    Msg = jsx:encode(JSON),
    lager:debug("[~p] <- ~s~n", [State#state.node, Msg]),
    {reply, {text, Msg}, State}.
