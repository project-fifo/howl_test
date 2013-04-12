-module(rt_howl).


-export([node_endpoing/1,
         call/2]).

-export([ws_open/1,
         ws_close/1,
         ws_auth/3,
         ws_join/2,
         ws_leave/2,
         ws_messages/1,
         wait_until_ws_message/2,
         listeners/2,
         url/1]).

node_endpoing(Node) ->
    {ok, IP} = rpc:call(Node, application, get_env, [mdns_server_lib, ip]),
    {ok, Port} = rpc:call(Node, application, get_env, [mdns_server_lib, port]),
    {IP, Port}.

call(Node, Msg) ->
    {IP, Port} = node_endpoing(Node),
    lager:debug("~s:~p <- ~p", [IP, Port, Msg]),
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {active,false}, {packet,4}], 100),
    ok = gen_tcp:send(Socket, term_to_binary(Msg)),
    {ok, Repl} = gen_tcp:recv(Socket, 0),
    {reply, Res} = binary_to_term(Repl),
    lager:debug("~s:~p -> ~p", [IP, Port, Res]),
    gen_tcp:close(Socket),
    Res.

listeners(Node, Channel) ->
    rpc:call(Node, howl, listeners, [Channel]).

url(Node) ->
    {ok, Port} = rpc:call(Node, application, get_env, [howl, http_port]),
    io_lib:format("ws://127.0.0.1:~p/howl", [Port]).

ws_open(Node) ->
    howl_test_ws_handler:connect(Node).

ws_close(Node) ->
    howl_test_ws_handler:close(Node).

ws_auth(WS, User, Pass) ->
    howl_test_ws_handler:auth(WS, User, Pass),
    wait_until_ws_message(WS, [{<<"ok">>, <<"authenticated">>}]).

ws_join(WS, Channel) ->
    howl_test_ws_handler:join(WS, Channel),
    wait_until_ws_message(WS, [{<<"ok">>, <<"channel joined">>}]).

ws_leave(WS, Channel) ->
    howl_test_ws_handler:leave(WS, Channel).

ws_messages(WS) ->
    howl_test_ws_handler:messages(WS).

wait_until_ws_message(Ws, Msg) ->
    lager:info("[ws: ~p] Waiting for: ~p", [Ws, Msg]),
    F = fun(_) ->
                {ok, Msgs} = ws_messages(Ws),
                lists:member(Msg, Msgs)
        end,
    rt:wait_until(Ws, F).
