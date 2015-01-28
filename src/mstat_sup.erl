-module(mstat_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SystemMonitor = {
        mstat_system_monitor,
        {mstat_system_monitor, start_link, []},
        permanent, 5000, worker, [mstat_system_monitor]
    },
    {ok, {{one_for_one, 5, 10}, [SystemMonitor]}}.

