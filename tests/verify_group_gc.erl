-module(verify_group_gc).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(GROUP1, <<"group1">>).


%% Group bucket size is set to two.
confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UUID} = rt_snarl:group_add(Node, ?GROUP1),
    %% Create permissions and check them
    ?assertEqual(ok, rt_snarl:group_grant(Node, UUID, [<<"p1">>])),
    ?assertEqual(ok, rt_snarl:group_grant(Node, UUID, [<<"p2">>])),
    ?assertEqual(ok, rt_snarl:group_grant(Node, UUID, [<<"p3">>])),
    %% Revoke two permissions
    ?assertEqual(ok, rt_snarl:group_revoke(Node, UUID, [<<"p1">>])),
    ?assertEqual(ok, rt_snarl:group_revoke(Node, UUID, [<<"p2">>])),
    {ok, Ps1} = rpc:call(Node, snarl_group, gcable, [UUID]),
    ?assertEqual(1, length(Ps1)),
    ?assertEqual(ok, rt_snarl:group_revoke(Node, UUID, [<<"p3">>])),
    {ok, Ps2} = rpc:call(Node, snarl_group, gcable, [UUID]),
    ?assertEqual(1, length(Ps2)),
    {ok, GCed} = rpc:call(Node, snarl_group, gc, [UUID, Ps2]),
    lager:debug("GC freed ~p bytes.", [GCed]),
    {ok, Ps3} = rpc:call(Node, snarl_group, gcable, [UUID]),
    ?assertEqual(0, length(Ps3)),
    pass.
