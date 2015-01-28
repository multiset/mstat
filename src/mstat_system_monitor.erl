-module(mstat_system_monitor).
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(st, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    load_monitors(),
    {ok, #st{}}.

handle_call(load_monitors, _From, State) ->
    load_monitors(),
    {reply, ok, State};
handle_call(Msg, _From, State) ->
    lager:info("~p got unknown call ~p", [?MODULE, Msg]),
    {reply, unknown_call, State}.

handle_cast(Msg, State) ->
    lager:info("~p got unknown cast ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info({monitor, Pid, long_gc, Info}, State) ->
    mstat:increment_counter([system_monitor, long_gc_count]),
    GCTime = proplists:get_value(timeout, Info),
    HeapSize = proplists:get_value(heap_size, Info),
    lager:warning("long_gc ~p ~p ms ~p bytes", [Pid, GCTime, HeapSize]),
    {noreply, State};
handle_info({monitor, PidOrPort, long_schedule, Info}, State) ->
    mstat:increment_counter([system_monitor, long_schedule_count]),
    Time = proplists:get_value(timeout, Info),
    case proplists:get_value(port_op, Info) of
        undefined ->
            %% PidOrPort is a Pid
            InLoc = proplists:get_value(in, Info),
            OutLoc = proplists:get_value(out, Info),
            lager:warning(
                "long_schedule pid ~p ~p ms at ~p (in) ~p (out)",
                [PidOrPort, Time, InLoc, OutLoc]
            );
        Op ->
            %% PidOrPort is a Port
            lager:warning(
                "long_schedule port ~p ~p ms at ~p",
                [PidOrPort, Time, Op]
            )
    end,
    {noreply, State};
handle_info({monitor, Pid, large_heap, Info}, State) ->
    mstat:increment_counter([system_monitor, large_heap_count]),
    HeapSize = proplists:get_value(heap_size, Info),
    lager:warning("large_heap ~p ~p bytes", [Pid, HeapSize]),
    {noreply, State};
handle_info({monitor, Pid, busy_port, Port}, State) ->
    mstat:increment_counter([system_monitor, busy_port_count]),
    lager:warning("busy_port ~p ~p", [Pid, Port]),
    {noreply, State};
handle_info({monitor, Pid, busy_dist_port, Port}, State) ->
    mstat:increment_counter([system_monitor, busy_dist_port_count]),
    lager:warning("busy_dist_port ~p ~p", [Pid, Port]),
    {noreply, State};
handle_info(Msg, State) ->
    lager:info("~p got unknown info ~p", [?MODULE, Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load_monitors() ->
    {ok, Opts} = application:get_env(mstat, system_monitors),
    ok = load_monitors(Opts),
    erlang:system_monitor(self(), Opts).

load_monitors([]) ->
    ok;
load_monitors([{long_gc, Threshold}|Opts]) ->
    mstat:new_counter([system_monitor, long_gc_count]),
    mstat:new_gauge([system_monitor, long_gc_threshold]),
    mstat:update_gauge([system_monitor, long_gc_threshold], Threshold),
    load_monitors(Opts);
load_monitors([{long_schedule, Threshold}|Opts]) ->
    mstat:new_counter([system_monitor, long_schedule_count]),
    mstat:new_gauge([system_monitor, long_schedule_threshold]),
    mstat:update_gauge([system_monitor, long_schedule_threshold], Threshold),
    load_monitors(Opts);
load_monitors([{large_heap, Threshold}|Opts]) ->
    mstat:new_counter([system_monitor, large_heap_count]),
    mstat:new_gauge([system_monitor, large_heap_threshold]),
    mstat:update_gauge([system_monitor, large_heap_threshold], Threshold),
    load_monitors(Opts);
load_monitors([busy_port|Opts]) ->
    mstat:new_counter([system_monitor, busy_port_count]),
    load_monitors(Opts);
load_monitors([busy_dist_port|Opts]) ->
    mstat:new_counter([system_monitor, busy_dist_port_count]),
    load_monitors(Opts).
