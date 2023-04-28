%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% 路由
%%% @end
%%% Created : 26. 4月 2023 13:59
%%%-------------------------------------------------------------------
-module(buzz_router).
-author("fluent").

%% API
-export([get_routes/0, open/0]).

get_routes() ->
  {ok, HostBuzz} = application:get_env(buzz, host),
  [{HostBuzz, [
    {"/user/register", user_handler, #{action => register}},
    {"/user/login", user_handler, #{action => login}},
    {"/user/logout", user_handler, #{action => logout}}
  ]}].

%% 放开的路由
open() ->
  [
    <<"/user/register">>,
    <<"/user/login">>
  ].