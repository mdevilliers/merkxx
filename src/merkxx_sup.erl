
-module(merkxx_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    MesosWorker = ?CHILD(merkxx_mesos_worker, worker),
    Store = ?CHILD(merkxx_request, worker),
    {ok, { {one_for_one, 5, 10}, [MesosWorker, Store]} }.

