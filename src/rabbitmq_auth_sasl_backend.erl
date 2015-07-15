%%%-------------------------------------------------------------------
%%% @author mateuszkorszun
%%% @copyright (C) 2015, LShift
%%% @doc
%%%
%%% @end
%%% Created : 09. Jul 2015 4:12 PM
%%%-------------------------------------------------------------------
-module(rabbitmq_auth_sasl_backend).
-author("mateuszkorszun").

-behaviour(rabbit_authn_backend).
-behaviour(rabbit_authz_backend).

%% API - rabbit_authn_backend
-export([user_login_authentication/2]).

%% API - rabbit_authz_backend
-export([user_login_authorization/1, check_vhost_access/3, check_resource_access/3]).

-include("logger.hrl").
-include("rabbitmq_auth_sasl.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

%%%===================================================================
%%% Authn callbacks
%%%===================================================================

user_login_authentication(_, []) -> exit(missing_client_cert);
user_login_authentication(undefined, _) -> exit(missing_token);
user_login_authentication(Token, [{cert, Cert}]) ->
  case rabbitmq_auth_sasl_token:validate(Token, Cert) of
    {ok, #external_user{username = Username, roles = Roles} = ExtUser} ->
      ?INFO("Token validated successfuly for: ~p", [Username]),
      {ok, build_auth_user(Username, Roles, ExtUser)};
    {error, invalid_token} ->
      ?ERR("Invalid token or user not authorized"),
      {refused, "invalid token", []};
    {error, Error} ->
      ?ERR("Failed to validate token: ~p", [Error]),
      {refused, "failed to validate token", []}
  end.

build_auth_user(Username, Roles, ExtUser) ->
  #auth_user{username = Username, tags = Roles, impl = [{external_user, ExtUser}]}.

%%%===================================================================
%%% Authz callbacks
%%%===================================================================

user_login_authorization(_Username) ->
  {ok, {?MODULE, none}}.

check_vhost_access(#auth_user{username = Username, impl = [{external_user, #external_user{permissions = Permissions}}]}, VHostPath, _) ->
  ?INFO("Checking vhost ~p permissions for ~p", [VHostPath, Username]),
  length(proplists:get_value(VHostPath, Permissions, [])) > 0;
check_vhost_access(#auth_user{username = Username}, _, _) ->
  ?ERR("Empty permissions for ~p", [Username]),
  false.

check_resource_access(#auth_user{username = Username}, #resource{virtual_host = VHostPath, name = Name}, Permission) ->
  ?INFO("Checking resource access ~p ~p ~p ~p", [Username, VHostPath, Name, Permission]),
  true.

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

check_vhost_access_test() ->
  U1 = #auth_user{username = <<"U">>, impl = impl([{<<"/">>, [{a, <<".*">>}]}])},
  ?assertEqual(true, check_vhost_access(U1, <<"/">>, undefined)),
  U2 = #auth_user{username = <<"U">>, impl = impl([{<<"/">>, []}])},
  ?assertEqual(false, check_vhost_access(U2, <<"/">>, undefined)),
  U3 = #auth_user{username = <<"U">>, impl = impl([])},
  ?assertEqual(false, check_vhost_access(U3, <<"/">>, undefined)),
  U4 = #auth_user{username = <<"U">>, impl = impl([{<<"/vhost">>, [{a, <<".*">>}]}])},
  ?assertEqual(false, check_vhost_access(U4, <<"/">>, undefined)).

impl(Perms) ->
  [{external_user, #external_user{permissions = Perms}}].


%%%===================================================================
%%%===================================================================
%%%===================================================================









