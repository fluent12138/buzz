%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 4月 2023 19:52
%%%-------------------------------------------------------------------
-module(blog_repo).
-author("fluent").
-include("../include/log.hrl").
-include("../include/blog.hrl").
%% API
-export([save_blog/4, is_exist_blog/2, update_blog/3, logic_delete/1]).

save_blog(Uid, Author, Title, Content) ->
  Sql = "INSERT INTO blog (user_id, author, title, content) VALUES (?, ?, ?, ?)",
  mysql_pool:query(Sql, [Uid, Author, Title, Content]).

update_blog(BlogId, Title, Content) ->
  KV = [{?TITLE, Title}, {?CONTENT, Content}],
  mysql_pool:update(?BLOG, BlogId, KV).

is_exist_blog(BlogId, Uid) ->
  Sql = "SELECT 1 FROM blog WHERE id = (?) AND user_id = (?)",
  {ok, _, Res}  = mysql_pool:query(Sql, [BlogId, Uid]),
  Res.

logic_delete(BlogId) ->
  mysql_pool:update(?BLOG, BlogId, ?IS_DELETE, 1).