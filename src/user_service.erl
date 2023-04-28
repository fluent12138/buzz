%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% user具体业务逻辑处理
%%% @end
%%% Created : 26. 4月 2023 15:20
%%%-------------------------------------------------------------------
-module(user_service).
-author("fluent").
-include("../include/Re.hrl").
-include("../include/log.hrl").
-include("../include/error.hrl").
-include("../include/user.hrl").
%% API
-export([do_register/1, do_login/1, do_logout/1, refreshtoken/1]).

do_register(#{<<"nickname">> := NickName}) when byte_size(NickName) < 4; byte_size(NickName) > 16 ->
  {error, "昵称长度小于4或大于16", ?PARAMS_ERROR};
do_register(#{<<"username">> := UserName}) when byte_size(UserName) < 4; byte_size(UserName) > 16 ->
  {error, "用户名长度小于4或大于16", ?PARAMS_ERROR};
do_register(#{<<"password">> := P1, <<"checkPassword">> := P2}) when P1 =/= P2 ->
  {error, "两次输入密码不一致", ?PARAMS_ERROR};
do_register(#{<<"password">> := P}) when byte_size(P) < 6; byte_size(P) > 16 ->
  {error, "密码长度小于6或大于16", ?PARAMS_ERROR};
do_register(#{<<"checkPassword">> := P}) when byte_size(P) < 6; byte_size(P) > 16 ->
  {error, "确认密码长度小于6或大于16", ?PARAMS_ERROR};
do_register(#{<<"gender">> := G}) when G /= <<"男"/utf8>>, G /= <<"女"/utf8>> ->
  {error, "性别输入有误", ?PARAMS_ERROR};
do_register(#{<<"username">> := UserName, <<"nickname">> := NickName, <<"password">> := PassWord, <<"gender">> := Gender}) ->
  %% 匹配特殊字符
  ReRes = case {re:run(UserName, ?RE_ACCOUNT, [global]), re:run(NickName, ?RE_ACCOUNT, [global])} of
    {{match, _}, {match, _}} -> ok;
    _ -> {error, "用户名或昵称包含特殊字符", ?PARAMS_ERROR}
  end,
  IsExist = user_repo:is_exist_user(UserName, NickName),
  case {ReRes, IsExist} of
    {{error, _, _}, _} -> % 说明存在特殊字符
      ReRes;
    {_, {[], []}} -> % 说明没有相同昵称和名字的用户
      user_repo:save_user(UserName, NickName, PassWord, Gender),
      ok;
    {_, {_, []}} -> {error, "用户名已存在", ?PARAMS_ERROR};
    {_, {[], _}} -> {error, "昵称已存在", ?PARAMS_ERROR};
    {_, {_, _}} -> {error, "用户名与昵称已存在", ?PARAMS_ERROR}
  end;
do_register(_) ->
  {error, "请求参数错误", ?NULL_ERROR}.

do_login(#{<<"username">> := UserName, <<"password">> := PassWord}) ->
  case user_repo:login(UserName, PassWord) of
    [] -> {error, "账号或密码错误", ?PARAMS_ERROR};
    [Data = [ID, _, _, _, _, _]] ->
      user_repo:change_user_status(ID, ?ONLINE),
      generate_userinfo(Data)
  end;

do_login(_) ->
  {error, "请求参数错误", ?NULL_ERROR}.

do_logout(State) ->
  ID = maps:get(current_uid, State),
  user_repo:change_user_status(ID, ?OFFLINE).

refreshtoken(#{<<"rft">> := RefreshToken}) ->
  case token_func:decrypt_token(RefreshToken) of
    {ok, Id, _ExpireAt, <<"rtk">>} ->
      Data = [{<<"token">>, token_func:encrypt_token(Id)}],
      {ok, Data};
    {error, Code, Msg, _Map} ->
      {error, Msg, Code}
  end;
refreshtoken(_) ->
  {error, "请重新登录", ?PARAMS_ERROR}.

generate_userinfo([Id, NickName, UserName, Avatar, Gender, Role]) ->
  {ok,
    [
      {<<"token">>, token_func:encrypt_token(Id)},
      {<<"refreshtoken">>, token_func:encrypt_refreshtoken(Id)},
      {<<"uid">>, Id},
      {<<"nickname">>, NickName},
      {<<"avatar">>, Avatar},
      {<<"username">>, UserName},
      {<<"gender">>, Gender},
      {<<"role">>, Role}
    ]
  }.