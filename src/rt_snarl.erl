-module(rt_snarl).


-export([node_endpoing/1,
         call/2]).


-export([
         auth/3,
         allowed/3,
         version/1
        ]).

-export([
         token_delete/2
        ]).

-export([
         user_lookup/2,
         user_list/1,
         user_cache/2,
         user_get/2,
         user_add/2,
         user_delete/2,
         user_grant/3,
         user_revoke/3,
         user_passwd/3,
         user_join/3,
         user_leave/3,
         user_set/3,
         user_set/4
        ]).

-export([
         group_list/1,
         group_get/2,
         group_add/2,
         group_delete/2,
         group_grant/3,
         group_revoke/3,
         group_set/3,
         group_set/4
        ]).


node_endpoing(Node) ->
    {ok, IP} = rpc:call(Node, application, get_env, [mdns_server_lib, ip]),
    {ok, Port} = rpc:call(Node, application, get_env, [mdns_server_lib, port]),
    {IP, Port}.

call(Node, Msg) ->
    {IP, Port} = node_endpoing(Node),
    lager:debug("~s:~p <- ~p", [IP, Port, Msg]),
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {active,false}, {packet,4}], 100),
    ok = gen_tcp:send(Socket, term_to_binary(Msg)),
    {ok, Repl} = gen_tcp:recv(Socket, 0),
    {reply, Res} = binary_to_term(Repl),
    lager:debug("~s:~p -> ~p", [IP, Port, Res]),
    gen_tcp:close(Socket),
    Res.

version(Node) ->
    ServerVersion = call(Node,version),
    ServerVersion.

auth(Node, User, Pass) ->
    call(Node,{user, auth, User, Pass}).

allowed(Node, User, Permission) ->
    call(Node,{user, allowed, User, Permission}).

%%%===================================================================
%%% Token Functions
%%%===================================================================

token_delete(Node, Token) ->
    call(Node,{token, delete, Token}).

%%%===================================================================
%%% User Functions
%%%===================================================================

user_set(Node, User, Attribute, Value) when
      is_binary(User) ->
    call(Node,{user, set, User, Attribute, Value}).

user_set(Node, User, Attributes) when
      is_binary(User) ->
    call(Node,{user, set, User, Attributes}).

user_list(Node) ->
    call(Node,{user, list}).

user_get(Node, User) ->
    call(Node,{user, get, User}).

user_lookup(Node, User) ->
    call(Node,{user, lookup, User}).

user_cache(Node, User) ->
    call(Node,{user, cache, User}).

user_add(Node, User) ->
    call(Node,{user, add, User}).

user_delete(Node, User) ->
    call(Node,{user, delete, User}).

user_grant(Node, User, Permission) ->
    call(Node,{user, grant, User, Permission}).

user_revoke(Node, User, Permission) ->
    call(Node,{user, revoke, User, Permission}).

user_passwd(Node, User, Pass) ->
    call(Node,{user, passwd, User, Pass}).

user_join(Node, User, Group) ->
    call(Node,{user, join, User, Group}).

user_leave(Node, User, Group) ->
    call(Node,{user, leave, User, Group}).

%%%===================================================================
%%% Group Functions
%%%===================================================================

group_set(Node, Group, Attribute, Value) when
      is_binary(Group) ->
    call(Node,{group, set, Group, Attribute, Value}).

group_set(Node, Group, Attributes) when
      is_binary(Group) ->
    call(Node,{group, set, Group, Attributes}).

group_list(Node) ->
    call(Node,{group, list}).

group_get(Node, Group) ->
    call(Node,{group, get, Group}).

group_add(Node, Group) ->
    call(Node,{group, add, Group}).

group_delete(Node, Group) ->
    call(Node,{group, delete, Group}).

group_grant(Node, Group, Permission) ->
    call(Node,{group, grant, Group, Permission}).

group_revoke(Node, Group, Permission) ->
    call(Node,{group, revoke, Group, Permission}).
