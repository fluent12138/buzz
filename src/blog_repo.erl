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
-export([save_blog/4]).
-export([is_exist_blog/2]).
-export([update_blog/3]).
-export([logic_delete/1]).
-export([get_blog_count/0, get_blog_count/1]).
-export([page/2, query/3]).

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

page(Limit, Offset) ->
  Sql = <<"SELECT ", ?PAGE_COL/binary, "FROM blog WHERE is_delete = 0 LIMIT  ? OFFSET ?">>,
  mysql_pool:query(Sql, [Limit, Offset]).

get_blog_count() ->
  Sql = <<"SELECT COUNT(id) FROM blog WHERE is_delete = 0">>,
  {ok, _, [[Res]]} = mysql_pool:query(Sql),
  Res.

get_blog_count(Uid) ->
  Sql = <<"SELECT COUNT(id) FROM blog WHERE is_delete = 0 AND user_id = (?)">>,
  {ok, _, [[Res]]} = mysql_pool:query(Sql, [Uid]),
  Res.

query(Limit, Offset, Uid) ->
  Sql = <<"SELECT ", ?PAGE_COL/binary, "FROM blog WHERE is_delete = 0 AND user_id = (?) LIMIT  ? OFFSET ?">>,
  mysql_pool:query(Sql, [Uid, Limit, Offset]).
