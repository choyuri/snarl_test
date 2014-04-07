-module(verify_user_permissions).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).
-define(ROLE1, <<"role">>).
-define(P1, [<<"some">>, <<"permission">>]).
-define(P2, [<<"some">>, <<"other">>, <<"permission">>]).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UUIDU} = rt_snarl:user_add(Node, ?USER1),
    ?assertEqual(ok, rt_snarl:user_grant(Node, UUIDU, ?P1)),
    ?assertEqual(true, rt_snarl:allowed(Node, UUIDU, ?P1)),

    {ok, UUIDG} = rt_snarl:role_add(Node, ?ROLE1),
    ?assertEqual(ok, rt_snarl:role_grant(Node, UUIDG, ?P2)),
    ?assertEqual(false, rt_snarl:allowed(Node, UUIDU, ?P2)),
    ?assertEqual(ok, rt_snarl:user_join(Node, UUIDU, UUIDG)),
    ?assertEqual(true, rt_snarl:allowed(Node, UUIDU, ?P2)),
    pass.
