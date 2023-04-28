%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% 为user模块提供工具函数
%%% @end
%%% Created : 26. 4月 2023 20:30
%%%-------------------------------------------------------------------
-module(user_func).
-author("fluent").
-include("../include/user.hrl").
%% API
-export([hash/1]).

hash(Password) ->
  Hash = crypto:hash(md5, concat(?SALT, Password)),
  erlang:binary_to_list(Hash).

concat(S1, S2) ->
  <<S1/binary, S2/binary>>.


