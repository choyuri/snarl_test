-module(verify_user_keys).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).
-define(KID1, <<"id 1">>).
-define(KID2, <<"id 2">>).
-define(K1, <<"key 1">>).
-define(K2, <<"key 2">>).


confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UUID} = rt_snarl:user_add(Node, ?USER1),
    ?assertEqual({ok, []}, rt_snarl:user_keys(Node, UUID)),
    ?assertEqual(ok, rt_snarl:user_key_add(Node, UUID, ?KID1, ?K1)),
    ?assertEqual({ok, [{?KID1, ?K1}]}, rt_snarl:user_keys(Node, UUID)),
    ?assertEqual(ok, rt_snarl:user_key_add(Node, UUID, ?KID2, ?K2)),
    ?assertEqual({ok, [{?KID1, ?K1}, {?KID2, ?K2}]}, rt_snarl:user_keys(Node, UUID)),
    ?assertEqual(ok, rt_snarl:user_key_revoke(Node, UUID, ?KID1)),
    ?assertEqual({ok, [{?KID2, ?K2}]}, rt_snarl:user_keys(Node, UUID)),
    pass.
    %% ?assertEqual(ok, rt_snarl:user_grant(Node, UUIDU, ?P1)),
    %% ?assertEqual(true, rt_snarl:allowed(Node, UUIDU, ?P1)),

    %% {ok, UUIDG} = rt_snarl:group_add(Node, ?GROUP1),
    %% ?assertEqual(ok, rt_snarl:group_grant(Node, UUIDG, ?P2)),
    %% ?assertEqual(false, rt_snarl:allowed(Node, UUIDU, ?P2)),
    %% ?assertEqual(ok, rt_snarl:user_join(Node, UUIDU, UUIDG)),
    %% ?assertEqual(true, rt_snarl:allowed(Node, UUIDU, ?P2)),
