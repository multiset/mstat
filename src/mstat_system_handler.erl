-module(mstat_system_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req0, State) ->
    Data = mstat_util:ejsonify([{vm, vm()}, {processes, procs()}]),
    {ok, Req1} = cowboy_req:reply(200, [], jiffy:encode(Data), Req0),
    {ok, Req1, State}.

procs() ->
    {ok, Sups} = application:get_env(mstat, supervisors),
    lists:map(
        fun(Name) ->
            PI0 = proc(whereis(Name)),
            PI1 = case lists:member(Name, Sups) of
                false ->
                    PI0;
                true ->
                    [{supervising, supervised_procs(Name)}|PI0]
            end,
            {Name, PI1}
        end,
        registered()
    ).

supervised_procs(Name) ->
    Children = try
        [P || {_, P, _, _} <- supervisor:which_children(Name)]
    catch error:{noproc, _} ->
        []
    end,
    [PI|PIs] = lists:map(fun proc/1, Children),
    ZipFun = fun
        ({K, X}, {K, Y}) when is_list(Y) -> {K, [X|Y]};
        ({K, X}, {K, Y}) -> {K, [X, Y]}
    end,
    Zipped = lists:foldl(fun(A, B) -> lists:zipwith(ZipFun, A, B) end, PI, PIs),
    lists:map(fun({K, Vs}) -> {K, bear:get_statistics(Vs)} end, Zipped).

proc(undefined) ->
    [];
proc(Pid) ->
    Keys = [
        binary,
        heap_size,
        links,
        memory,
        message_queue_len,
        min_bin_vheap_size,
        min_heap_size,
        monitored_by,
        monitors,
        priority,
        reductions,
        stack_size,
        status,
        total_heap_size
    ],
    PI = process_info(Pid, Keys),
    lists:map(
        fun(Key) -> {Key, proc_prop(lists:keyfind(Key, 1, PI))} end,
        Keys
    ).

proc_prop({binary, B}) ->
    lists:sum([S || {_, S, _} <- B]);
proc_prop({links, L}) ->
    length(L);
proc_prop({monitors, M}) ->
    length(M);
proc_prop({monitored_by, M}) ->
    length(M);
proc_prop({priority, low}) ->
    1;
proc_prop({priority, normal}) ->
    2;
proc_prop({priority, high}) ->
    3;
proc_prop({priority, max}) ->
    4;
proc_prop({priority, _}) ->
    -1;
proc_prop({status, exiting}) ->
    0;
proc_prop({status, garbage_collecting}) ->
    1;
proc_prop({status, waiting}) ->
    2;
proc_prop({status, running}) ->
    3;
proc_prop({status, runnable}) ->
    4;
proc_prop({status, suspended}) ->
    5;
proc_prop({status, _}) ->
    -1;
proc_prop({_, N}) ->
    N.

vm() ->
    %% TODO: Consider adding scheduler_wall_time
    {ContextSwitches, 0} = statistics(context_switches),
    {GCCount, WordsReclaimed, 0} = statistics(garbage_collection),
    GCParams = erlang:system_info(garbage_collection),
    PortIO = tuple_to_list(statistics(io)),
    PortCount = erlang:system_info(port_count),
    PortLimit = erlang:system_info(port_limit),
    {Reductions, _} = statistics(reductions),
    RunQueueList = tuple_to_list(statistics(run_queues)),
    RunQueues = lists:map(
        fun(N) -> {integer_to_binary(N), lists:nth(N, RunQueueList)} end,
        lists:seq(1, length(RunQueueList))
    ),
    {Uptime, _} = statistics(wall_clock),
    CheckIO = erlang:system_info(check_io),
    [
        {context_switches, ContextSwitches},
        {garbage_collection,
            [{count, GCCount}|[{words, WordsReclaimed}|GCParams]]
        },
        {ports, [{io, PortIO}, {count, PortCount}, {limit, PortLimit}]},
        {reductions, Reductions},
        {run_queues, RunQueues},
        {uptime, Uptime},
        {fds, [
            {max, proplists:get_value(max_fds, CheckIO)},
            {active, proplists:get_value(active_fds, CheckIO)}
        ]},
        {zdbbl, erlang:system_info(dist_buf_busy_limit)},
        {ets, [
            {limit, erlang:system_info(ets_limit)},
            {active, length(ets:all())}
        ]},
        {processes, [
            {count, erlang:system_info(process_count)},
            {limit, erlang:system_info(process_limit)}
        ]},
        {schedulers, [
            {count, erlang:system_info(schedulers)},
            {online, erlang:system_info(schedulers_online)}
        ]},
        {async_thread_pool_size, erlang:system_info(thread_pool_size)},
        {memory, erlang:memory()}
    ].
