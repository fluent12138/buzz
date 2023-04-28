-define(SALT, <<"FLUENT">>).
%% token有效期 2小时 单位毫秒 7200000 = 3600 * 1000 * 2
-define(TOKEN_VALID, 7200000).
%% refreshtoken有效期 10天 单位毫秒 864000000 = 86400 * 1000 * 10
-define(REFRESHTOKEN_VALID, 864000000).
%% jwt key
-define (JWT_KEY, <<"Jv6uGx8tDyK1lOq2sZmPcWkRiNp0aXhE">>).

%% user status
-define(ONLINE, 1).
-define(OFFLINE, 0).
-define(ADMIN, <<"1">>).
-define(USER, <<"0">>).
%% sql
-define(USERINFO_COL, <<"id, nickname, username, avatar, gender, role ">>).