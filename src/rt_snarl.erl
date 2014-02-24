-module(rt_snarl).

-export([node_endpoing/1,
         call/2]).

-export([
         auth/3,
         auth/4,
         allowed/3,
         version/1
        ]).

-export([
         token_delete/2
        ]).

-export([
         user_add/2,
         user_cache/2,
         user_delete/2,
         user_get/2,
         user_grant/3,
         user_join/3,
         user_key_add/4,
         user_key_revoke/3,
         user_keys/2,
         user_leave/3,
         user_list/1,
         user_lookup/2,
         user_passwd/3,
         user_revoke/3,
         user_set/3,
         user_set/4,
         user_join_org/3,
         user_leave_org/3,
         user_select_org/3,
         user_active_org/2,
         user_orgs/2
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

-export([
         org_add/2,
         org_delete/2,
         org_get/2,
         org_add_trigger/3,
         org_list/1,
         org_remove_trigger/3,
         org_execute_trigger/4,
         org_set/3,
         org_set/4
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

auth(Node, User, Pass, Otp) ->
    call(Node, libsnarl_msg:auth(User, Pass, Otp)).

auth(Node, User, Pass) ->
    call(Node, libsnarl_msg:auth(User, Pass)).

allowed(Node, User, Permission) ->
    call(Node, libsnarl_msg:allowed(User, Permission)).

%%%===================================================================
%%% Token Functions
%%%===================================================================

token_delete(Node, Token) ->
    call(Node, libsnarl_msg:token_delete(Token)).

%%%===================================================================
%%% User Functions
%%%===================================================================

user_set(Node, User, Attribute, Value) when
      is_binary(User) ->
    call(Node, libsnarl_msg:user_set(User, Attribute, Value)).

user_set(Node, User, Attributes) when
      is_binary(User) ->
    call(Node,libsnarl_msg:user_set(User, Attributes)).

user_list(Node) ->
    call(Node,libsnarl_msg:user_list()).

user_get(Node, User) ->
    call(Node,libsnarl_msg:user_get(User)).

user_lookup(Node, User) ->
    call(Node,libsnarl_msg:user_lookup(User)).

user_cache(Node, User) ->
    call(Node,libsnarl_msg:user_cache(User)).

user_add(Node, User) ->
    call(Node,libsnarl_msg:user_add(User)).

user_delete(Node, User) ->
    call(Node,libsnarl_msg:user_delete(User)).

user_grant(Node, User, Permission) ->
    call(Node,libsnarl_msg:user_grant(User, Permission)).

user_revoke(Node, User, Permission) ->
    call(Node,libsnarl_msg:user_revoke(User, Permission)).

user_passwd(Node, User, Pass) ->
    call(Node,libsnarl_msg:user_passwd(User, Pass)).

user_join(Node, User, Group) ->
    call(Node,libsnarl_msg:user_join(User, Group)).

user_leave(Node, User, Group) ->
    call(Node,libsnarl_msg:user_leave(User, Group)).

user_key_add(Node, User, KeyID, Key) ->
    call(Node, libsnarl_msg:user_key_add(User, KeyID, Key)).

user_key_revoke(Node, User, KeyID) ->
    call(Node, libsnarl_msg:user_key_revoke(User, KeyID)).

user_keys(Node, User) ->
    call(Node, libsnarl_msg:user_keys(User)).

user_join_org(Node, User, Org) ->
    call(Node,libsnarl_msg:user_join_org(User, Org)).

user_leave_org(Node, User, Org) ->
    call(Node,libsnarl_msg:user_leave_org(User, Org)).

user_select_org(Node, User, Org) ->
    call(Node,libsnarl_msg:user_select_org(User, Org)).

user_active_org(Node, User) ->
    call(Node, libsnarl_msg:user_active_org(User)).

user_orgs(Node, User) ->
    call(Node, libsnarl_msg:user_orgs(User)).

%%%===================================================================
%%% Group Functions
%%%===================================================================

group_set(Node, Group, Attribute, Value) when
      is_binary(Group) ->
    call(Node,libsnarl_msg:group_set(Group, Attribute, Value)).

group_set(Node, Group, Attributes) when
      is_binary(Group) ->
    call(Node,libsnarl_msg:group_set(Group, Attributes)).

group_list(Node) ->
    call(Node,libsnarl_msg:group_list()).

group_get(Node, Group) ->
    call(Node,libsnarl_msg:group_get(Group)).

group_add(Node, Group) ->
    call(Node,libsnarl_msg:group_add(Group)).

group_delete(Node, Group) ->
    call(Node,libsnarl_msg:group_delete(Group)).

group_grant(Node, Group, Permission) ->
    call(Node,libsnarl_msg:group_grant(Group, Permission)).

group_revoke(Node, Group, Permission) ->
    call(Node,libsnarl_msg:group_revoke(Group, Permission)).

%%%===================================================================
%%% org Functions
%%%===================================================================

org_set(Node, Org, Attribute, Value) when
      is_binary(Org) ->
    call(Node, libsnarl_msg:org_set(Org, Attribute, Value)).

org_set(Node, Org, Attributes) when
      is_binary(Org) ->
    call(Node, libsnarl_msg:org_set(Org, Attributes)).

org_list(Node) ->
    call(Node, libsnarl_msg:org_list()).

org_get(Node, Org) ->
    call(Node, libsnarl_msg:org_get(Org)).

org_add(Node, Org) ->
    call(Node, libsnarl_msg:org_add(Org)).

org_delete(Node, Org) ->
    call(Node, libsnarl_msg:org_delete(Org)).

org_add_trigger(Node, Org, Trigger) ->
    call(Node, libsnarl_msg:org_add_trigger(Org, Trigger)).

org_remove_trigger(Node, Org, Trigger) ->
    call(Node, libsnarl_msg:org_remove_trigger(Org, Trigger)).

org_execute_trigger(Node, Org, Event, Payload) ->
    call(Node, libsnarl_msg:org_execute_trigger(Org, Event, Payload)).
