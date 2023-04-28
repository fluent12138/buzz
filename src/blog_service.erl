%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% blog具体业务逻辑处理
%%% @end
%%% Created : 26. 4月 2023 15:20
%%%-------------------------------------------------------------------
-module(blog_service).
-author("fluent").
-include("../include/log.hrl").
-include("../include/error.hrl").
-include("../include/user.hrl").
-include("../include/blog.hrl").
%% API
-export([save/2, delete/3]).

save(#{<<"title">> := Title}, _) when byte_size(Title) =:= 0 ->
  {error, "标题不能为空", ?PARAMS_ERROR};
save(#{<<"title">> := Title}, _) when byte_size(Title) > 16 ->
  {error, "标题长度大于16", ?PARAMS_ERROR};
save(#{<<"content">> := Content}, _) when byte_size(Content) =:= 0 ->
  {error, "博客内容不能为空", ?PARAMS_ERROR};
save(#{<<"content">> := Content}, _) when byte_size(Content) > ?CONTENT_MAX_LEN ->
  {error, "博客内容超出最大容量限制", ?PARAMS_ERROR};
%% 新增blog
save(#{<<"author">> := Author, <<"title">> := Title, <<"content">> := Content}, Uid) ->
  ?LOG({Title, Content}),
  case user_repo:is_exist_user(Uid, Author) of
    [] -> {error, "不存在该用户, 请确认输出信息是否正确", ?PARAMS_ERROR};
    _ ->
      blog_repo:save_blog(Uid, Author, Title, Content)
  end;
%% 更新blog
save(#{<<"id">> := BlogId, <<"title">> := Title, <<"content">> := Content}, Uid) ->
  ?LOG({BlogId, Title, Content, Uid}),
  case blog_repo:is_exist_blog(BlogId, Uid) of
    [] -> {error, "没有修改权限", ?NO_AUTH};
    _ ->
      blog_repo:update_blog(BlogId, Title, Content)
  end;
save(_, _) ->
  {error, "请求参数为空", ?NULL_ERROR}.

delete(Role, _, #{<<"id">> := BlogId}) when Role == ?ADMIN ->
  blog_repo:logic_delete(BlogId);
delete(_, Uid, #{<<"id">> := BlogId}) ->
  case blog_repo:is_exist_blog(BlogId, Uid) of
    [] -> {error, "没有修改权限", ?NO_AUTH};
    _ ->
      blog_repo:logic_delete(BlogId)
  end;
delete(_, _, _) ->
  {error, "请求参数为空", ?NULL_ERROR}.