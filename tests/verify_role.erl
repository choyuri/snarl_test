-module(verify_role).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(ROLE1, <<"role1">>).
-define(ROLE2, <<"role2">>).
-define(USER1, <<"user1">>).

%% Test if role permissions are propperly propagated:
%% 2) Check

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),

    %% Set up a user and two roles.
    {ok, R1} = rt_snarl:role_add(Node, ?ROLE1),
    {ok, R2} = rt_snarl:role_add(Node, ?ROLE2),
    {ok, U} = rt_snarl:user_add(Node, ?USER1),

    %% Create user and see if direct permissions work and non of the
    %% others do.
    ?assertEqual(
       ok, rt_snarl:user_grant(Node, U,
                               [<<"some">>, <<"user">>, <<"permission">>])),
    test_permissions(Node, U, true, false, false),

    %% Add a user to a role with preexisting permissions
    ?assertEqual(
       ok, rt_snarl:role_grant(Node, R1,
                               [<<"some">>, <<"role">>, <<"permission">>])),
    ?assertEqual(
       ok, rt_snarl:user_join(Node, U, R1)),

    %% Now the original permission should be working and the R1 permissions
    %% Should work too.
    test_permissions(Node, U, true, true, false),

    %% Add a user to a role without permissions and add a permission later
    ?assertEqual(
       ok, rt_snarl:user_join(Node, U, R2)),
    %% Permissions should not have changed.
    test_permissions(Node, U, true, true, false),
    %% Now we grant a permission to the newly added role and it should be
    %% working.
    ?assertEqual(
       ok, rt_snarl:role_grant(Node, R2,
                               [<<"other">>, <<"role">>, <<"permission">>])),
    test_permissions(Node, U, true, true, true),
    pass.

test_permissions(N, U, T1, T2, T3) ->
    ?assertEqual(
       T1, rt_snarl:allowed(N, U,
                            [<<"some">>, <<"user">>, <<"permission">>])),
    ?assertEqual(
       T2, rt_snarl:allowed(N, U,
                            [<<"some">>, <<"role">>, <<"permission">>])),
    ?assertEqual(
       T3, rt_snarl:allowed(N, U,
                            [<<"other">>, <<"role">>, <<"permission">>])).
