%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 4月 2023 13:56
%%%-------------------------------------------------------------------
-module(user_handler).
-author("fluent").
-include("../include/log.hrl").
%% API
-export([init/2, terminate/3]).

init(Req0, State0) ->
  Action = maps:get(action, State0),
  State = maps:remove(action, State0),
  Req = case Action of
          register -> user_register(Req0, State);
          login -> user_login(Req0, State);
          logout -> user_logout(Req0, State);
          false -> Req0
         end,
  {ok, Req, State}.

user_register(Req, _State) ->
  Data = buzz_req_func:get_req_body(Req),
  case user_service:do_register(Data) of
    ok -> buzz_response:success(Req);
    {error, Msg, Code} -> buzz_response:error(Req, Msg, Code)
  end.

user_login(Req, _State) ->
  Data = buzz_req_func:get_req_body(Req),
  case user_service:do_login(Data) of
    {ok, UserInfo} -> buzz_response:success(Req, UserInfo);
    {error, Msg, Code} -> buzz_response:error(Req, Msg, Code)
  end.

user_logout(Req, State) ->
  user_service:do_logout(State),
  buzz_response:success(Req).

terminate(_Reason, _Req, _State) ->
  ok.