-module(user_ensure_unique_name).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(USER1, <<"user1">>).

confirm() ->
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    {ok, _UUID1} = rt_snarl:user_add(Node, ?USER1),
    ?assertEqual(duplicate, rt_snarl:user_add(Node, ?USER1)),
    pass.
