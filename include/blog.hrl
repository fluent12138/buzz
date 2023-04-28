%% content最多为16MB
-define(CONTENT_MAX_LEN, 16777215).
%% sql
-define(BLOG, <<"blog">>).
-define(TITLE, <<"title">>).
-define(CONTENT, <<"content">>).
-define(IS_DELETE, <<"is_delete">>).
-define(PAGE_COL, <<"id, user_id, author, title, content, like_count, dislike_count, collect_count, create_time ">>).