%%%-------------------------------------------------------------------
%%% @author mateuszkorszun
%%% @copyright (C) 2015, LShift
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2015 1:19 PM
%%%-------------------------------------------------------------------
-module(rabbitmq_auth_sasl_token).
-author("mateuszkorszun").

-export([validate/2]).

-define(HTTPC_OPTS, [{version, "HTTP/1.1"}]).
-define(CONTENT_TYPE, "application/json").
-define(B2A(B), list_to_atom(binary_to_list(B))).

-include("logger.hrl").
-include("rabbitmq_auth_sasl.hrl").

%%%===================================================================
%%% Token validation API
%%%===================================================================

-spec validate(Token :: any(), Cert :: binary()) -> {ok, User :: #external_user{}}
| {error, invalid_token} | {error, Reason :: any()}.
validate(Token, Cert) ->
  {ok, ValidationServiceConf} = application:get_env(rabbitmq_auth_sasl, token_validation_service),
  ValidationServiceURL = proplists:get_value(url, ValidationServiceConf),
  Params = {ValidationServiceURL, [], ?CONTENT_TYPE, request_body(Token, Cert)},

  ?INFO("Calling validation service at ~p", [ValidationServiceURL]),
  case httpc:request(post, Params, ?HTTPC_OPTS, []) of
    {ok, {{_HTTP, 200, _}, _Headers, Body}} ->
      {ok, build_user(Body)};
    {ok, {{_HTTP, 401, _}, _Headers, Body}} ->
      {error, invalid_token};
    {ok, {{_HTTP, Code, _}, _Headers, Body}} ->
      {error, {Code, Body}};
    {error, _} = E ->
      E
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_user(Body) ->
  {struct, Resp} = mochijson2:decode(Body),
  Username = proplists:get_value(<<"username">>, Resp),
  Roles = proplists:get_value(<<"roles">>, Resp, []),
  Permissions = element(2, proplists:get_value(<<"permissions">>, Resp, [])),

  #external_user{
    username = Username,
    roles = build_roles(Roles),
    permissions = build_permissions(Permissions)
  }.

build_roles(Roles) ->
  [?B2A(R) || R <- Roles].

build_permissions([]) -> [];
build_permissions([{VHost, {struct, Permissions}} | H]) ->
  [{VHost, build_permissions2(Permissions)} | build_permissions(H)].

build_permissions2([]) -> [];
build_permissions2([{P, R} | T]) ->
  [{?B2A(P), R} | build_permissions2(T)].

request_body(Token, Cert) ->
  mochijson2:encode({struct, [{token, Token}, {cert, binary_to_list(Cert)}]}).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

build_user_test() ->
  RawBody = "{\"username\":\"Guest\",\"roles\":[\"a\", \"b\"],\"permissions\":{\"\\/\":{\"p1\":\".*\",\"p2\":\".*\"}}}",
  #external_user{username = U, roles = R, permissions = P} = build_user(RawBody),

  ?assertEqual(<<"Guest">>, U),
  ?assertEqual([a, b], R),
  ?assertEqual([{<<"/">>, [{p1, <<".*">>}, {p2, <<".*">>}]}], P).

build_user_with_empty_permissions_test() ->
  RawBody = "{\"username\":\"Guest\",\"roles\":[\"a\", \"b\"],\"permissions\":{}}",
  #external_user{username = U, roles = R, permissions = P} = build_user(RawBody),

  ?assertEqual(<<"Guest">>, U),
  ?assertEqual([a, b], R),
  ?assertEqual([], P).

%%%===================================================================
%%%===================================================================
%%%===================================================================