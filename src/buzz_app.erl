%%%-------------------------------------------------------------------
%% @doc buzz public API
%% @end
%%%-------------------------------------------------------------------

-module(buzz_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    before_start(),
    Routes = buzz_router:get_routes(),
    Dispatch = cowboy_router:compile(Routes),
    {ok, Port} = application:get_env(buzz, http_port),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        middlewares => [
            cowboy_router,
            % verify_middleware,
            auth_middleware,
            cowboy_handler
        ],
        env => #{
            dispatch => Dispatch
        }
    }),

    case buzz_sup:start_link() of
        {ok, Pid} ->
            io:format("start buzz ...~n"),
            {ok, Pid};
        Error ->
            Error
    end.

before_start() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok.

stop(_State) ->
    ok.

%% internal functions
