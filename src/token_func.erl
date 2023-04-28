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
-include("../include/error.hrl").
%% API
-export([encrypt_refreshtoken/1, encrypt_token/1, decrypt_token/1]).
-type token_type() :: rtk | tk.

%% 生成refreshtoken
encrypt_refreshtoken(Info) ->
  encrypt_token(Info, ?REFRESHTOKEN_VALID, rtk).

%% 生成token
encrypt_token(Info) ->
  encrypt_token(Info, ?TOKEN_VALID, tk).

%% 解析token
decrypt_token(Token) ->
  try
    jwerl:verify(Token, hs256, ?JWT_KEY)
  of
    {ok, Payload} ->
      Uid = maps:get(uid, Payload, 0),
      Role = maps:get(role, Payload, 0),
      ID = Uid,
      ExpireAt = maps:get(exp, Payload, 0),
      Sub = maps:get(sub, Payload, 0),
      {ok, ID, Role, ExpireAt, Sub};
    {error, _JWT_ERR} ->
      {error, ?EXPIRED_ERROR, "请刷新token", []};
    _JWT_ERR ->
      {error, ?NO_AUTH, "token无效", []}
  catch
    _:_ ->
      {error, ?NO_AUTH, "token无效", []}
  end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% 生成token
-spec encrypt_token(iodata(), integer(), token_type()) -> any().
encrypt_token(Info, Millisecond, Sub) when is_map(Info)->
  ID = maps:get(id, Info),
  case maps:is_key(role, Info) of
    false -> encrypt_token(ID, Millisecond, Sub);
    true ->
      Role = maps:get(role, Info),
      encrypt_token(ID, Role, Millisecond, Sub)
  end;

encrypt_token(ID, Millisecond, Sub) when is_integer(ID) ->
  ID2 = integer_to_binary(ID),
  encrypt_token(ID2, Millisecond, Sub);

encrypt_token(ID, Millisecond, Sub)->
  ExpireAt = millisecond() + Millisecond,
  Data = #{
    sub => Sub,
    exp => ExpireAt,
    uid => ID},
  jwerl:sign(Data, hs256, ?JWT_KEY).

encrypt_token(ID, Role, Millisecond, Sub) when is_integer(ID) ->
  ID2 = integer_to_binary(ID),
  encrypt_token(ID2, Role, Millisecond, Sub);

encrypt_token(ID, Role, Millisecond, Sub)->
  ExpireAt = millisecond() + Millisecond,
  Data = #{
    sub => Sub,
    exp => ExpireAt,
    uid => ID,
    role => Role},
  jwerl:sign(Data, hs256, ?JWT_KEY).

millisecond() ->
  os:system_time(millisecond).