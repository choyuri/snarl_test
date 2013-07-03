-module(verify_user_gc).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).
-define(GROUP1, <<"group1">>).
-define(GROUP2, <<"group2">>).
-define(GROUP3, <<"group3">>).



%% User bucket size is set to two.
confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UUID} = rt_snarl:user_add(Node, ?USER1),
    %% Create permissions and check them
    ?assertEqual(ok, rt_snarl:user_grant(Node, UUID, [<<"p1">>])),
    ?assertEqual(true, rt_snarl:allowed(Node, UUID, [<<"p1">>])),
    ?assertEqual(ok, rt_snarl:user_grant(Node, UUID, [<<"p2">>])),
    ?assertEqual(true, rt_snarl:allowed(Node, UUID, [<<"p2">>])),
    ?assertEqual(ok, rt_snarl:user_grant(Node, UUID, [<<"p3">>])),
    ?assertEqual(true, rt_snarl:allowed(Node, UUID, [<<"p3">>])),
    %% Revoke two permissions
    ?assertEqual(ok, rt_snarl:user_revoke(Node, UUID, [<<"p1">>])),
    ?assertEqual(false, rt_snarl:allowed(Node, UUID, [<<"p1">>])),
    ?assertEqual(ok, rt_snarl:user_revoke(Node, UUID, [<<"p2">>])),
    ?assertEqual(false, rt_snarl:allowed(Node, UUID, [<<"p2">>])),
    {ok, {Ps1, Gs1, Ks}} = rpc:call(Node, snarl_user, gcable, [UUID]),
    ?assertEqual(1, length(Ps1)),
    ?assertEqual(0, length(Gs1)),
    ?assertEqual(0, length(Ks)),
    {ok, G1} = rt_snarl:group_add(Node, ?GROUP1),
    ?assertEqual(ok, rt_snarl:user_join(Node, UUID, G1)),
    {ok, G2} = rt_snarl:group_add(Node, ?GROUP2),
    ?assertEqual(ok, rt_snarl:user_join(Node, UUID, G2)),
    {ok, G3} = rt_snarl:group_add(Node, ?GROUP3),
    ?assertEqual(ok, rt_snarl:user_join(Node, UUID, G3)),
    ?assertEqual(ok, rt_snarl:user_leave(Node, UUID, G1)),
    ?assertEqual(ok, rt_snarl:user_leave(Node, UUID, G3)),
    {ok, {Ps2, Gs2, Ks}} = rpc:call(Node, snarl_user, gcable, [UUID]),
    ?assertEqual(1, length(Ps2)),
    ?assertEqual(1, length(Gs2)),
    ?assertEqual(ok, rt_snarl:user_revoke(Node, UUID, [<<"p3">>])),
    ?assertEqual(ok, rt_snarl:user_leave(Node, UUID, G2)),
    {ok, {Ps3, Gs3, Ks}} = rpc:call(Node, snarl_user, gcable, [UUID]),
    ?assertEqual(1, length(Ps3)),
    ?assertEqual(1, length(Gs3)),
    {ok, GCed} = rpc:call(Node, snarl_user, gc, [UUID, {Ps3, Gs3, Ks}]),
    lager:debug("GC freed ~p bytes.", [GCed]),
    {ok, {Ps4, Gs4, Ks}} = rpc:call(Node, snarl_user, gcable, [UUID]),
    ?assertEqual(0, length(Ps4)),
    ?assertEqual(0, length(Gs4)),
    pass.
