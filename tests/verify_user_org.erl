-module(verify_user_org).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).
-define(ORG1, <<"org1">>).
-define(GROUP1, <<"grop1">>).


confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, UserID} = rt_snarl:user_add(Node, ?USER1),
    {ok, GroupID} = rt_snarl:group_add(Node, ?GROUP1),
    {ok, OrgID} = rt_snarl:org_add(Node, ?ORG1),
    Event = vm_create,
    Trigger = {vm_create,
               {grant, group, GroupID,
                [<<"some">>, placeholder, <<"permission">>]}},
    Payload = <<"some id">>,
    ?assertEqual(ok, rt_snarl:user_join(Node, UserID, GroupID)),
    ?assertEqual({ok, []}, rt_snarl:user_orgs(Node, UserID)),
    ?assertEqual({ok, <<"">>}, rt_snarl:user_active_org(Node, UserID)),
    ?assertEqual(ok, rt_snarl:user_join_org(Node, UserID, OrgID)),
    ?assertEqual({ok, [OrgID]}, rt_snarl:user_orgs(Node, UserID)),
    ?assertEqual({ok, <<"">>}, rt_snarl:user_active_org(Node, UserID)),
    ?assertEqual(ok, rt_snarl:user_select_org(Node, UserID, OrgID)),
    ?assertEqual({ok, OrgID}, rt_snarl:user_active_org(Node, UserID)),

    ?assertEqual(false,
                 rt_snarl:allowed(
                   Node, UserID, [<<"some">>, Payload, <<"permission">>])),

    ?assertEqual(ok, rt_snarl:org_add_trigger(Node, OrgID, Trigger)),

    ?assertEqual({ok, 1},
                 rt_snarl:org_execute_trigger(Node, OrgID, Event, Payload)),

    ?assertEqual(true,
                 rt_snarl:allowed(
                   Node, UserID, [<<"some">>, Payload, <<"permission">>])),
    pass.
