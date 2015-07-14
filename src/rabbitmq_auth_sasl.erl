%%%-------------------------------------------------------------------
%%% @author mateuszkorszun
%%% @copyright (C) 2015, LShift
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2015 2:14 PM
%%%-------------------------------------------------------------------
-module(rabbitmq_auth_sasl).
-author("mateuszkorszun").

-behaviour(rabbit_auth_mechanism).

%% Auth mechanism callbacks
-export([description/0, should_offer/1, init/1, handle_response/2]).

-record(state, {token = undefined, cert = undefined}).

-rabbit_boot_step({?MODULE,
  [{description, "SASL"},
    {mfa, {rabbit_registry, register,
      [auth_mechanism, <<"EXTERNAL">>, ?MODULE]}},
    {requires, rabbit_registry},
    {enables, kernel_ready},
    {cleanup, {rabbit_registry, unregister,
      [auth_mechanism, <<"EXTERNAL">>]}}]}).

-include("logger.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("public_key/include/public_key.hrl").

%%%===================================================================
%%% Auth mechanism callbacks
%%%===================================================================

description() ->
  [{description, <<"Token based authentication mechanism using SASL EXTERNAL">>}].

should_offer(Sock) ->
  ?INFO("Chacking SSL cert..."),
  case rabbit_net:peercert(Sock) of
    nossl ->
      ?ERR("No SSL cert found"),
      false;
    {error, no_peercert} ->
      ?INFO("No per cert found"),
      true;
    {ok, _} ->
      ?INFO("SSL cert present"),
      true
  end.

init(Sock) ->
  ?INFO("Initializing auth state"),
  case rabbit_net:peercert(Sock) of
    {ok, C} ->
      #state{cert = C};
    {error, no_peercert} ->
      {refused, none, "no peer certificate", []};
    nossl ->
      {refused, none, "not SSL connection", []}
  end.

handle_response(_Response, {refused, _, Reason, _} = E) ->
  ?ERR("Connection refused: ~p", [Reason]),
  E;

handle_response(<<>>, #state{token = undefined} = State) ->
  ?INFO("Starting SASL handshake. Requesting token"),
  {challenge, <<"Token required">>, State};

handle_response(Token, #state{cert = Cert}) ->
  ?INFO("Finishing SASL handshake. Token provided: ~p", [Token]),
  rabbit_access_control:check_user_login(Token, [{cert, Cert}]).

%%%===================================================================
%%%===================================================================
%%%===================================================================