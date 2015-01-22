-module(mstat_metrics_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req0, State) ->
    Metrics = lists:map(
        fun(Name) -> {Name, mstat:sample(Name)} end,
        mstat:list()
    ),
    {ok, Req1} = cowboy_req:reply(
        200,
        [],
        jiffy:encode(mstat_util:ejsonify(Metrics)),
        Req0
    ),
    {ok, Req1, State}.
