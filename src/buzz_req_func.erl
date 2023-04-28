%%%-------------------------------------------------------------------
%%% @author fluent
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% 请求相关工具函数
%%% @end
%%% Created : 27. 4月 2023 11:38
%%%-------------------------------------------------------------------
-module(buzz_req_func).
-author("fluent").

%% API
-export([get_req_body/1]).

%% convert to map
get_req_body(Req) ->
  {ok, Data, _} = cowboy_req:read_body(Req),
  case Data of
    <<>> -> <<>>;
    _ -> jsx:decode(Data, [{return_maps, true}])
  end.