%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 4月 2023 19:52
%%%-------------------------------------------------------------------
-module(user_repo).
-author("fluent").
-include("../include/user.hrl").
-include("../include/log.hrl").
%% API
-export([is_exist_user/2, save_user/4, login/2, change_user_status/2]).

%% 判断是是否存在用户, 并视情况返回信息
is_exist_user(Uid, NickName) when is_integer(Uid)->
  Sql = "SELECT 1 FROM user WHERE id = (?) AND nickname = (?)",
  {ok, _, Res}  = mysql_pool:query(Sql, [Uid, NickName]),
  Res;
is_exist_user(UserName, NickName) ->
  Sql1 = "SELECT id FROM user WHERE username = (?)",
  Sql2 = "SELECT id FROM user WHERE nickname = (?)",
  {ok, _, Res1} = mysql_pool:query(Sql1, [UserName]),
  {ok, _, Res2} = mysql_pool:query(Sql2, [NickName]),
  {Res1, Res2}.

save_user(UserName, NickName, PassWord, Gender) ->
  Sql = "INSERT INTO user (username, nickname, password, gender) VALUES (?, ?, ?, ?)",
  mysql_pool:query(Sql,  [UserName, NickName, user_func:hash(PassWord), Gender]).

login(UserName, PassWord) ->
  Where = <<"WHERE username = (?) AND password = (?) ">>,
  Sql = <<"SELECT ", ?USERINFO_COL/binary, "FROM user ", Where/binary>>,
  {ok, _, Res} = mysql_pool:query(Sql,  [UserName, user_func:hash(PassWord)]),
  Res.

change_user_status(ID, Status) ->
  mysql_pool:update(<<"user">>, ID, <<"status">>, Status).