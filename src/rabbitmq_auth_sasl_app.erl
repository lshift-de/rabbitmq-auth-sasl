%%%-------------------------------------------------------------------
%%% @author mateuszkorszun
%%% @copyright (C) 2015, LShift
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2015 3:04 PM
%%%-------------------------------------------------------------------
-module(rabbitmq_auth_sasl_app).
-author("mateuszkorszun").

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-include("logger.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(normal, []) ->
  ?INFO("Starting SASL plugin"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ?INFO("Stoping SASL plugin"),
  ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  {ok, {{one_for_one, 3, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
