%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% jwt功能函数
%%% @end
%%% Created : 27. 4月 2023 16:07
%%%-------------------------------------------------------------------
-module(token_func).
-author("fluent").
-include("../include/user.hrl").
-include("../include/log.hrl").
%% API
-export([encrypt_refreshtoken/1, encrypt_token/1, decrypt_token/1]).
-type token_type() :: rtk | tk.

%% 生成refreshtoken
encrypt_refreshtoken(ID) ->
  encrypt_token(ID, ?REFRESHTOKEN_VALID, rtk).

%% 生成token
encrypt_token(ID) ->
  encrypt_token(ID, ?TOKEN_VALID, tk).

%% 解析token
decrypt_token(Token) ->
  try
    jwerl:verify(Token, hs256, ?JWT_KEY)
  of
    {ok, Payload} ->
      Uid = maps:get(uid, Payload, 0),
      ID = Uid,
      ExpireAt = maps:get(exp, Payload, 0),
      Sub = maps:get(sub, Payload, 0),
      {ok, ID, ExpireAt, Sub};
    {error, _JWT_ERR} ->
      {error, 705, "请刷新token", []};
    _JWT_ERR ->
      {error, 706, "token无效", []}
  catch
    _:_ ->
      {error, 706, "token无效", []}
  end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% 生成token
-spec encrypt_token(iodata(), integer(), token_type()) -> any().
encrypt_token(ID, Millisecond, Sub) when is_integer(ID) ->
  ID2 = integer_to_binary(ID),
  encrypt_token(ID2, Millisecond, Sub);
encrypt_token(ID, Millisecond, Sub) ->
  ExpireAt = millisecond() + Millisecond,
  Data = #{
    % iss => buzz  % iss (issuer)：签发人
    % , nbf => Now + 1 % nbf (Not Before)：生效时间
    % , iat => Now % iat (Issued At)：签发时间
    sub => Sub  % sub (subject)：主题
    ,
    exp => ExpireAt  % exp (expiration time)：过期时间
    ,
    uid => ID},
  jwerl:sign(Data, hs256, ?JWT_KEY).

millisecond() ->
  os:system_time(millisecond).