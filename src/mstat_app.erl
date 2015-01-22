-module(mstat_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
        [{'_', [
            {"/metrics", mstat_metrics_handler, []},
            {"/system", mstat_system_handler, []}
        ]}]
    ),
    {ok, Port} = application:get_env(mstat, port),
    {ok, Address} = application:get_env(mstat, address),
    {ok, _Pid} = cowboy:start_http(
        mstat,
        5,
        [{port, Port}, {ip, Address}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    mstat_sup:start_link().

stop(_State) ->
    ok.
