-module(verify_org_metadata).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(ORG1, <<"org1">>).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),

    %% Set up a org, role and org.
    {ok, OrgID} = rt_snarl:org_add(Node, ?ORG1),

    %% Verify we can set metadata
    ?assertEqual(ok, rt_snarl:org_set(Node, OrgID, <<"key">>, 1)),
    ?assertEqual([{<<"key">>, 1}], metadata(Node, OrgID)),

    %% Verify we can change metadata
    ?assertEqual(ok, rt_snarl:org_set(Node, OrgID, <<"key">>, 2)),
    ?assertEqual([{<<"key">>, 2}], metadata(Node, OrgID)),

    %% Verify we can nested metadata
    ?assertEqual(ok, rt_snarl:org_set(Node, OrgID, <<"key2.value">>, 3)),
    ?assertEqual(ok, rt_snarl:org_set(Node, OrgID, [<<"key2">>, <<"value2">>], 4)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 4}]}],
                 metadata(Node, OrgID)),
    ?assertEqual(ok, rt_snarl:org_set(Node, OrgID, [<<"key2">>, <<"value2">>], 5)),
    ?assertEqual([{<<"key">>, 2},
                  {<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, OrgID)),

    %% Verify delete metadata
    ?assertEqual(ok, rt_snarl:org_set(Node, OrgID, <<"key">>, delete)),
    ?assertEqual([{<<"key2">>,
                   [{<<"value">>, 3},
                    {<<"value2">>, 5}]}],
                 metadata(Node, OrgID)),


    pass.

metadata(Node, OrgID) ->
    {ok, U} = rt_snarl:org_get(Node, OrgID),
    {ok, M} = orddict:find(<<"metadata">>, U),
    M.
