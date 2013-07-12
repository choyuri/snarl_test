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

    %% Set up a user, group and org.
    {ok, UserID} = rt_snarl:user_add(Node, ?USER1),
    {ok, GroupID} = rt_snarl:group_add(Node, ?GROUP1),
    {ok, OrgID} = rt_snarl:org_add(Node, ?ORG1),

    %% Set up some trigger examples.
    Event = vm_create,
    Trigger = {Event,
               {grant, group, GroupID,
                [<<"some">>, placeholder, <<"permission">>]}},
    Payload = <<"some id">>,

    %% set up the basics.
    ?assertEqual(ok, rt_snarl:user_join(Node, UserID, GroupID)),
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
    pass.
