-module(verify_auth).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UUID1} = rt_snarl:user_add(Node, ?USER1),
    ?assertEqual(ok, rt_snarl:user_passwd(Node, UUID1, ?USER1)),
    Res1 = rt_snarl:auth(Node, ?USER1, ?USER1),
    ?assertNotEqual(not_found, Res1),
    Res2 = rt_snarl:auth(Node, UUID1, ?USER1, basic),
    ?assertEqual({ok, UUID1}, Res2),
    pass.
