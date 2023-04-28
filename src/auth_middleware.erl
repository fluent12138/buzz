%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% 鉴权
%%% @end
%%% Created : 27. 4月 2023 15:31
%%%-------------------------------------------------------------------
-module(auth_middleware).
-author("fluent").
-behavior(cowboy_middleware).
-include("../include/user.hrl").
-include("../include/log.hrl").
-export([execute/2]).

%% 这个是回调函数
execute(Req, Env) ->
  Path = remove_last_forward_slash(cowboy_req:path(Req)),
  OpenRoutes = buzz_router:open(),
  InOpenRoutes = lists:member(Path, OpenRoutes), %判断是否在放开的路径中
  Authorization = cowboy_req:header(<<"authorization">>, Req),
  condition(InOpenRoutes, Authorization, Req, Env).

%% 如果放开了, 没有传auth, 直接通过, 传入了auth, 进行鉴权
%% 否则, 鉴权
condition(true, undefined, Req, Env) ->
  {ok, Req, Env};
condition(true, Auth, Req, Env) ->
  do_authorization(Auth, Req, Env);
condition(_, Auth, Req, Env) ->
  do_authorization(Auth, Req, Env).

%% 鉴权
do_authorization(Auth, Req, Env) ->
  case token_func:decrypt_token(Auth) of
    {ok, Id, Role, _ExpireAt, <<"tk">>} when is_integer(Id) ->
      #{handler_opts := HandlerOpts} = Env,
      Env2 = Env#{handler_opts := HandlerOpts#{current_uid => Id, role => Role}},
      {ok, Req, Env2};
    {ok, Id, Role, _ExpireAt, <<"tk">>} when is_binary(Id) ->
      #{handler_opts := HandlerOpts} = Env,
      Env2 = Env#{handler_opts := HandlerOpts#{current_uid => Id, role => Role}},
      {ok, Req, Env2};
    {ok, _Id, _ExpireAt, <<"rtk">>} ->
      Err = "Does not support refreshtoken",
      Req1 = buzz_response:error(Req, Err, 1),
      {stop, Req1};
    {error, Code, Msg, _Map} ->
      Req1 = buzz_response:error(Req, Msg, Code),
      {stop, Req1}
  end.

-spec remove_last_forward_slash(binary()) -> binary().
remove_last_forward_slash(<<"">>) ->
  <<"/">>;
remove_last_forward_slash(<<"/">>) ->
  <<"/">>;
remove_last_forward_slash(Path) ->
  case binary:last(Path) of
    47 ->
      binary:part(Path, 0, byte_size(Path)-1);
    _ ->
      Path
  end.