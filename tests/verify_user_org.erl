-module(verify_user_org).
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
    {ok, RoleID} = rt_snarl:role_add(Node, ?ROLE1),
    {ok, OrgID} = rt_snarl:org_add(Node, ?ORG1),

    %% Set up some trigger examples.
    Event = vm_create,
    Trigger = {Event,
               {grant, role, RoleID,
                [<<"some">>, placeholder, <<"permission">>]}},
    Payload = <<"some id">>,

    %% set up the basics.
    ?assertEqual(ok, rt_snarl:user_join(Node, UserID, RoleID)),
    ?assertEqual({ok, []}, rt_snarl:user_orgs(Node, UserID)),
    ?assertEqual({ok, <<"">>}, rt_snarl:user_active_org(Node, UserID)),
    ?assertEqual(ok, rt_snarl:user_join_org(Node, UserID, OrgID)),
    ?assertEqual({ok, [OrgID]}, rt_snarl:user_orgs(Node, UserID)),
    ?assertEqual({ok, <<"">>}, rt_snarl:user_active_org(Node, UserID)),
    ?assertEqual(ok, rt_snarl:user_select_org(Node, UserID, OrgID)),
    ?assertEqual({ok, OrgID}, rt_snarl:user_active_org(Node, UserID)),

    %% Tests if the user has no permissions.
    ?assertEqual(false,
                 rt_snarl:allowed(
                   Node, UserID, [<<"some">>, Payload, <<"permission">>])),

    %% Add the trigger for the org
    ?assertEqual(ok, rt_snarl:org_add_trigger(Node, OrgID, Trigger)),

    %% Execute the event - make sure one trigger is fired.
    ?assertEqual({ok, 1},
                 rt_snarl:org_execute_trigger(Node, OrgID, Event, Payload)),

    %% Test again if the permissions were granted
    ?assertEqual(true,
                 rt_snarl:allowed(
                   Node, UserID, [<<"some">>, Payload, <<"permission">>])),

    %% Test if leaving the org resets the active org.
    ?assertEqual(ok, rt_snarl:user_leave_org(Node, UserID, OrgID)),
    ?assertEqual({ok, []}, rt_snarl:user_orgs(Node, UserID)),
    ?assertEqual({ok, <<"">>}, rt_snarl:user_active_org(Node, UserID)),


    %% Rejoin and select the org.
    ?assertEqual(ok, rt_snarl:user_join_org(Node, UserID, OrgID)),
    ?assertEqual({ok, [OrgID]}, rt_snarl:user_orgs(Node, UserID)),
    ?assertEqual(ok, rt_snarl:user_select_org(Node, UserID, OrgID)),
    ?assertEqual({ok, OrgID}, rt_snarl:user_active_org(Node, UserID)),

    %% Check that we have 1 trigger
    ?assertEqual(1, length(org_triggers(Node, OrgID))),

    %% Deleting the role should delete the trigger and also delete the users
    %% accociation with the role.

    ?assertEqual(ok, rt_snarl:role_delete(Node, RoleID)),
    ?assertEqual(not_found, rt_snarl:role_get(Node, RoleID)),
    timer:sleep(500),
    ?assertEqual([], user_roles(Node, UserID)),
    ?assertEqual([], org_triggers(Node, OrgID)),


    %% Adding a user trigger to see if ti get deleted
    Trigger1 = {Event,
                {grant, user, UserID,
                 [<<"some">>, placeholder, <<"permission">>]}},
    ?assertEqual(ok, rt_snarl:org_add_trigger(Node, OrgID, Trigger1)),
    ?assertEqual(1, length(org_triggers(Node, OrgID))),

    %% See if the trigger gets deleted when the user is deleted
    ?assertEqual(ok, rt_snarl:user_delete(Node, UserID)),
    timer:sleep(500),
    ?assertEqual([], org_triggers(Node, OrgID)),

    %% Deleting the role should
    pass.


org_triggers(Node, OrgID) ->
    {ok, Org} = rt_snarl:org_get(Node, OrgID),
    {<<"triggers">>, Ts} = lists:keyfind(<<"triggers">>, 1, Org),
    Ts.

user_roles(Node, UserID) ->
    {ok, Org} = rt_snarl:user_get(Node, UserID),
    {<<"roles">>, Rs} = lists:keyfind(<<"roles">>, 1, Org),
    Rs.
