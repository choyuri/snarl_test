-module(token_fail_on_deleted_user).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UUID1} = rt_snarl:user_add(Node, ?USER1),
    UserObj = [{<<"groups">>,[]},
               {<<"keys">>,[]},
               {<<"metadata">>,[]},
               {<<"name">>,?USER1},
               {<<"password">>,
                <<207,125,68,5,102,30,39,44,20,28,215,184,
                  159,14,245,179,103,178,125,45>>},
               {<<"permissions">>,
                [[<<"user">>, UUID1, <<"...">>]]},
               {<<"uuid">>, UUID1}],
    ?assertEqual(ok, rt_snarl:user_passwd(Node, UUID1, ?USER1)),
    {ok, Token} = rt_snarl:auth(Node, ?USER1, ?USER1),
    ?assertEqual({ok, UserObj}, rt_snarl:user_get(Node, Token)),
    ?assertEqual(ok, rt_snarl:user_delete(Node, UUID1)),
    ?assertEqual(not_found, rt_snarl:user_get(Node, Token)),
    pass.
