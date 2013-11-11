-module(verify_message_delivery).
-behavior(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").


-define(C1, <<"ch1">>).
-define(USER, <<"user">>).
-define(PASS, <<"pass">>).

-define(MSG1, [{<<"k">>, <<"v">>}]).

confirm() ->
    lager:info("Deploying 4 nodes"),
    [Node1 | NodesRest] = Nodes = rt:build_cluster(4),

    lager:info("Creating a websocket connection for each node."),
    [_ | WSsRest] = WSs = [ connect_ws(N) || N <- Nodes],

    lager:info("Try delivery from each node."),
    [test_delivery(Node, WSs) || Node <- Nodes],

    lager:info("Bring down the first node and try if delivery works on the remaining nodes."),
    rt:stop(Node1),
    [test_delivery(Node, WSsRest) || Node <- NodesRest],

    lager:info("Bring the first node back up and try if delivery works."),
    rt:start_and_wait(Node1),
    rt:wait_for_service(Node1, rt:config(rc_services, [riak_kv])),
    NewWSs = [connect_ws(Node1) | WSsRest],
    [test_delivery(Node, NewWSs) || Node <- Nodes],

    pass.

connect_ws(Node) ->
    {ok, WS} = rt_howl:ws_open(Node),
    rt_howl:ws_auth(WS, ?USER, ?PASS),
    rt_howl:ws_join(WS, ?C1),
    rt_howl:ws_clear(WS),
    WS.

test_delivery(Node, WSs) ->
    lager:info("Testig delivery form node ~p.", [Node]),
    rt_howl:send(Node, ?C1, ?MSG1),
    [rt_howl:wait_until_ws_message(Ws) || Ws <- WSs],
    timer:sleep(1000),
    [?assertEqual(mk_reply(?C1, ?MSG1), rt_howl:ws_messages(Ws)) || Ws <- WSs],
    [rt_howl:ws_clear(Ws) || Ws <- WSs].

mk_reply(C, M) ->
    {ok, [[{<<"channel">>,C},
           {<<"message">>,M}]]}.
