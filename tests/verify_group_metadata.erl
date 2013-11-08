-module(verify_group_metadata).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(GROUP1, <<"group1">>).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),

    %% Set up a group, group and org.
    {ok, GroupID} = rt_snarl:group_add(Node, ?GROUP1),

    %% Verify we can set metadata
    ?assertEqual(ok, rt_snarl:group_set(Node, GroupID, <<"key">>, 1)),
    ?assertEqual([{<<"key">>, 1}], metadata(Node, GroupID)),

    %% Verify we can change metadata
    ?assertEqual(ok, rt_snarl:group_set(Node, GroupID, <<"key">>, 2)),
    ?assertEqual([{<<"key">>, 2}], metadata(Node, GroupID)),

    %% Verify we can nested metadata
    ?assertEqual(ok, rt_snarl:group_set(Node, GroupID, <<"key2.value">>, 3)),
    ?assertEqual(ok, rt_snarl:group_set(Node, GroupID, [<<"key2">>, <<"value2">>], 4)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 4}]}],
                 metadata(Node, GroupID)),
    ?assertEqual(ok, rt_snarl:group_set(Node, GroupID, [<<"key2">>, <<"value2">>], 5)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, GroupID)),

    %% Verify delete metadata
    ?assertEqual(ok, rt_snarl:group_set(Node, GroupID, <<"key">>, delete)),
    ?assertEqual([{<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, GroupID)),


    pass.

metadata(Node, GroupID) ->
    {ok, U} = rt_snarl:group_get(Node, GroupID),
    {ok, M} = orddict:find(<<"metadata">>, U),
    M.
