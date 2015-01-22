-module(mstat).

-export([
    list/0,
    sample/1,
    new_counter/1,
    new_gauge/1,
    new_histogram/1,
    decrement_counter/1,
    decrement_counter/2,
    increment_counter/1,
    increment_counter/2,
    update_gauge/2,
    update_histogram/2,
    timeit/2,
    timeit/3,
    timeit/4
]).

list() ->
    folsom_metrics:get_metrics().


sample(Name) ->
    [{Name, Info}] = folsom_metrics:get_metric_info(Name),
    sample_type(Name, proplists:get_value(type, Info)).


sample_type(Name, histogram) ->
    folsom_metrics:get_histogram_statistics(Name);
sample_type(Name, _) ->
    folsom_metrics:get_metric_value(Name).


new_counter(Name) ->
    folsom_metrics:new_counter(Name).


new_gauge(Name) ->
    folsom_metrics:new_gauge(Name).


new_histogram(Name) ->
    {ok, I} = application:get_env(mstat, histogram_window),
    folsom_metrics:new_histogram(Name, slide_uniform, {I, 1024}).


decrement_counter(Name) ->
    decrement_counter(Name, 1).


decrement_counter(Name, Value) ->
    catch folsom_metrics:notify_existing_metric(Name, {dec, Value}, counter).


increment_counter(Name) ->
    increment_counter(Name, 1).


increment_counter(Name, Value) ->
    catch folsom_metrics:notify_existing_metric(Name, {inc, Value}, counter).


update_gauge(Name, Value) ->
    catch folsom_metrics:notify_existing_metric(Name, Value, gauge).


update_histogram(Name, Value)  ->
    catch folsom_metrics:notify_existing_metric(Name, Value, histogram).


timeit(Name, Fun) ->
    timeit(Name, Fun, []).


timeit(Name, Fun, Args) ->
    T0 = os:timestamp(),
    Result = erlang:apply(Fun, Args),
    Duration = timer:now_diff(os:timestamp(), T0) div 1000,
    update_histogram(Name, Duration),
    Result.


timeit(Name, Module, Function, Args) ->
    T0 = os:timestamp(),
    Result = erlang:apply(Module, Function, Args),
    Duration = timer:now_diff(os:timestamp(), T0) div 1000,
    update_histogram(Name, Duration),
    Result.
