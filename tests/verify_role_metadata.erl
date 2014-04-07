-module(verify_role_metadata).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(ROLE1, <<"role1">>).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),

    %% Set up a role, role and org.
    {ok, RoleID} = rt_snarl:role_add(Node, ?ROLE1),

    %% Verify we can set metadata
    ?assertEqual(ok, rt_snarl:role_set(Node, RoleID, <<"key">>, 1)),
    ?assertEqual([{<<"key">>, 1}], metadata(Node, RoleID)),

    %% Verify we can change metadata
    ?assertEqual(ok, rt_snarl:role_set(Node, RoleID, <<"key">>, 2)),
    ?assertEqual([{<<"key">>, 2}], metadata(Node, RoleID)),

    %% Verify we can nested metadata
    ?assertEqual(ok, rt_snarl:role_set(Node, RoleID, <<"key2.value">>, 3)),
    ?assertEqual(ok, rt_snarl:role_set(Node, RoleID, [<<"key2">>, <<"value2">>], 4)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 4}]}],
                 metadata(Node, RoleID)),
    ?assertEqual(ok, rt_snarl:role_set(Node, RoleID, [<<"key2">>, <<"value2">>], 5)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, RoleID)),

    %% Verify delete metadata
    ?assertEqual(ok, rt_snarl:role_set(Node, RoleID, <<"key">>, delete)),
    ?assertEqual([{<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, RoleID)),


    pass.

metadata(Node, RoleID) ->
    {ok, U} = rt_snarl:role_get(Node, RoleID),
    {ok, M} = orddict:find(<<"metadata">>, U),
    M.
