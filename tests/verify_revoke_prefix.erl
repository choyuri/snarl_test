-module(verify_revoke_prefix).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).
-define(ROLE1, <<"role">>).
-define(PREFIX, [<<"another">>]).

-define(P1, [<<"user">>, <<"permission">>]).
-define(P2, [<<"another">>, <<"user">>, <<"permission">>]).

-define(P3, [<<"role">>, <<"permission">>]).
-define(P4, [<<"another">>, <<"role">>, <<"permission">>]).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UUIDU} = rt_snarl:user_add(Node, ?USER1),
    ?assertEqual(ok, rt_snarl:user_grant(Node, UUIDU, ?P1)),
    ?assertEqual(ok, rt_snarl:user_grant(Node, UUIDU, ?P2)),
    test_perms(Node, UUIDU, true, true, false, false),

    {ok, UUIDR} = rt_snarl:role_add(Node, ?ROLE1),
    ?assertEqual(ok, rt_snarl:role_grant(Node, UUIDR, ?P3)),
    ?assertEqual(ok, rt_snarl:role_grant(Node, UUIDR, ?P4)),
    ?assertEqual(ok, rt_snarl:user_join(Node, UUIDU, UUIDR)),
    test_perms(Node, UUIDU, true, true, true, true),

    ?assertEqual(ok, rt_snarl:user_revoke_prefix(Node, UUIDU, ?PREFIX)),
    test_perms(Node, UUIDU, true, false, true, true),

    ?assertEqual(ok, rt_snarl:role_revoke_prefix(Node, UUIDR, ?PREFIX)),
    test_perms(Node, UUIDU, true, false, true, false),

    pass.

test_perms(N, U, T1, T2, T3, T4) ->
    ?assertEqual(T1, rt_snarl:allowed(N, U, ?P1)),
    ?assertEqual(T2, rt_snarl:allowed(N, U, ?P2)),
    ?assertEqual(T3, rt_snarl:allowed(N, U, ?P3)),
    ?assertEqual(T4, rt_snarl:allowed(N, U, ?P4)).
