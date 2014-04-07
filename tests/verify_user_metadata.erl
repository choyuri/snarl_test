-module(verify_user_metadata).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).
-define(ORG1, <<"org1">>).
-define(ROLE1, <<"grop1">>).


confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),

    %% Set up a user, role and org.
    {ok, UserID} = rt_snarl:user_add(Node, ?USER1),

    %% Verify we can set metadata
    ?assertEqual(ok, rt_snarl:user_set(Node, UserID, <<"key">>, 1)),
    ?assertEqual([{<<"key">>, 1}], metadata(Node, UserID)),

    %% Verify we can change metadata
    ?assertEqual(ok, rt_snarl:user_set(Node, UserID, <<"key">>, 2)),
    ?assertEqual([{<<"key">>, 2}], metadata(Node, UserID)),

    %% Verify we can nested metadata
    ?assertEqual(ok, rt_snarl:user_set(Node, UserID, <<"key2.value">>, 3)),
    ?assertEqual(ok, rt_snarl:user_set(Node, UserID, [<<"key2">>, <<"value2">>], 4)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 4}]}],
                 metadata(Node, UserID)),
    ?assertEqual(ok, rt_snarl:user_set(Node, UserID, [<<"key2">>, <<"value2">>], 5)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, UserID)),

    %% Verify delete metadata
    ?assertEqual(ok, rt_snarl:user_set(Node, UserID, <<"key">>, delete)),
    ?assertEqual([{<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, UserID)),


    pass.

metadata(Node, UserID) ->
    {ok, U} = rt_snarl:user_get(Node, UserID),
    {ok, M} = orddict:find(<<"metadata">>, U),
    M.
