%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% blog handler
%%% @end
%%% Created : 26. 4月 2023 13:56
%%%-------------------------------------------------------------------
-module(blog_handler).
-author("fluent").
-include("../include/log.hrl").
-include("../include/user.hrl").
%% API
-export([init/2, terminate/3]).

init(Req0, State0) ->
  Action = maps:get(action, State0),
  State = maps:remove(action, State0),
  Req = case Action of
          add -> add(Req0, State);
          update -> update(Req0, State);
          delete -> delete(Req0, State);
          query -> query(Req0, State);
          display -> display(Req0);
          false -> Req0
         end,
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

add(Req, State) ->
  Blog = buzz_req_func:get_req_body(Req),
  Uid = binary_to_integer(maps:get(current_uid, State)),
  case blog_service:save(Blog, Uid) of
      ok -> buzz_response:success(Req);
      {error, Msg, Code} -> buzz_response:error(Req, Msg, Code)
  end.

update(Req, State) ->
  Blog = buzz_req_func:get_req_body(Req),
  Uid = maps:get(current_uid, State),
  case blog_service:save(Blog, Uid) of
    ok -> buzz_response:success(Req);
    {error, Msg, Code} -> buzz_response:error(Req, Msg, Code)
  end.

delete(Req, State) ->
  Blog = buzz_req_func:get_req_body(Req),
  Uid = maps:get(current_uid, State),
  Role = maps:get(role, State),
  case blog_service:delete(Role, Uid, Blog) of
    ok -> buzz_response:success(Req);
    {error, Msg, Code} -> buzz_response:error(Req, Msg, Code)
  end.

display(Req) ->
  Blog = buzz_req_func:get_req_body(Req),
  Payload = blog_service:display(Blog),
  buzz_response:success(Req, Payload).

query(Req, #{role := Role}) when Role =:= ?ADMIN ->
  Blog = buzz_req_func:get_req_body(Req),
  Payload = blog_service:query(Blog),
  buzz_response:success(Req, Payload);
query(Req, State) ->
  Blog = buzz_req_func:get_req_body(Req),
  Uid = maps:get(current_uid, State),
  Blog1 = Blog#{<<"user_id">> => Uid},
  Payload = blog_service:query(Blog1),
  buzz_response:success(Req, Payload).