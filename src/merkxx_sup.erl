
-module(merkxx_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(MasterLocation) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MasterLocation]).

init([MasterLocation]) ->

    MesosWorker ={ merkxx_mesos_worker, 
    				{scheduler, start_link, [merkxx_mesos_worker, MasterLocation]},
					permanent , brutal_kill, worker,[]},

    Store = ?CHILD(merkxx_request, worker),
    {ok, { {one_for_one, 5, 10}, [Store, MesosWorker]} }.

