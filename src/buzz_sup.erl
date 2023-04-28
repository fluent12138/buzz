%%%-------------------------------------------------------------------
%% @doc buzz top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(buzz_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  %% mysql
  {ok, SqlPool} = application:get_env(buzz, sqlPool),
  Name = proplists:get_value(name, SqlPool),
  PoolArgs = proplists:get_value(poolConf, SqlPool),
  WorkerArgs = proplists:get_value(sqlConf, SqlPool),
  Mysql = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
  {ok, {{one_for_all, 5, 10}, [Mysql]}}.

%% internal functions
