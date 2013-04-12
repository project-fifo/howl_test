-module(rt_howl).


-export([node_endpoing/1,
         call/2]).

-export([listen/2,
         leave/2,
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

listen(Node, Channel) ->
    rpc:call(Node, howl, listen, [Channel]).

leave(Node, Channel) ->
    rpc:call(Node, howl, leave, [Channel]).

listeners(Node, Channel) ->
    rpc:call(Node, howl, listeners, [Channel]).

url(Node) ->
    {ok, Port} = rpc:call(Node, application, get_env, [howl, http_port]),
    io_lib:format("ws://127.0.0.1:~p/howl", [Port]).
