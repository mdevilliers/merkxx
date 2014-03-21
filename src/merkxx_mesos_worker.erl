-module (merkxx_mesos_worker).

-behaviour (gen_server).
-behaviour (scheduler).

-include_lib("erlang_mesos/include/mesos_pb.hrl").

-export ([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% api
-export ([connect/1, disconnect/0]).

% from scheduler
-export ([registered/3, 
          reregistered/2, 
          disconnected/1, 
          offerRescinded/2, 
          statusUpdate/2, 
          frameworkMessage/4, 
          slaveLost/2, 
          executorLost/4, 
          error/2,
          resourceOffers/2]).

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

connect(MasterLocation) ->
    gen_server:cast(?MODULE, {connect, MasterLocation}).

disconnect() ->
    gen_server:cast(?MODULE, {disconnect}).

%% private
register_with_mesos(MasterLocation)->

    FrameworkInfo = #'FrameworkInfo'{user="", name="Merkxx"},
    MasterLocation = MasterLocation,
    MesosState = [],

    ok = scheduler:init(?MODULE, FrameworkInfo, MasterLocation, MesosState),
    {ok,Status} = scheduler:start(),
    Status.

disconnect_from_mesos() ->
    {ok,driver_stopped} = scheduler:stop(0), % stop the scheduler
    ok = scheduler:destroy(). % destroy and cleanup the nif

% MESOS call backs
registered(State, FrameworkID, MasterInfo) ->
    lager:info("Mesos::Registered callback : ~p ~p~n", [FrameworkID, MasterInfo]),
    {ok,State}.

reregistered(State, MasterInfo) ->
    lager:info("Mesos::ReRegistered callback : ~p ~n", [MasterInfo]),
    {ok,State}.

resourceOffers(State, Offer) ->
    lager:info("Mesos::ResourceOffers callback : ~p ~n", [Offer]),
    gen_server:cast(?MODULE, {resourceOffers, Offer}),
    {ok,State}.

disconnected(State) ->
    lager:info("Mesos::Disconnected callback"),
    {ok,State}.

offerRescinded(State, OfferID) ->
    lager:info("Mesos::OfferRescinded callback : ~p ~n", [OfferID]),
    {ok,State}.

statusUpdate(State, StatusUpdate) ->
    lager:info("Mesos::StatusUpdate callback : ~p ~n", [StatusUpdate]),
    {ok,State}. 

frameworkMessage(State, ExecutorID, SlaveID, Message) ->
    lager:info("Mesos::FrameworkMessage callback : ~p ~p ~p ~n", [ExecutorID, SlaveID, Message]),
    {ok,State}.

slaveLost(State, SlaveID) ->
    lager:info("Mesos::SlaveLost callback : ~p ~n", [SlaveID]),
    {ok,State}.

executorLost(State, ExecutorID, SlaveID, Status) ->
    lager:info("Mesos::ExecutorLost callback : ~p ~p ~p ~n", [ExecutorID, SlaveID, Status]),
    {ok,State}.

error(State, Message) ->
    lager:error("Mesos::Error callback : ~p ~n", [Message]),
    {ok,State}.

%% gen server API
init([]) ->
    lager:info("merkxx_mesos_worker started.", []),
    {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({resourceOffers, Offer}, State) ->
  
    Cpu = 1,
    Memory = 512,
    Ports = [],
    lager:info("Offer : ~p~n", [Offer]),
    case merkxx_request:match_next_request({Cpu, Memory, Ports }) of
         {ok, []} ->
            lager:info("Declining offer", []),
            scheduler:declineOffer(Offer#'Offer'.id);
         {ok, [ {provision_request, Identifier , Name, Command, _, _ , RequestedCpu , RequestedMemory , _ } ]} ->
            
                UniqueName = "Merkxx_" ++ Name ++ Identifier,
                Scalar = mesos_pb:enum_symbol_by_value('Value.Type', 0),
                CpuResource = #'Resource'{name="cpus", type=Scalar, scalar=#'Value.Scalar'{value=RequestedCpu}},
                MemoryResource = #'Resource'{name="mem", type=Scalar, scalar=#'Value.Scalar'{value=RequestedMemory}},
                %PortResource = #'Resource'{name="memory", type=Scalar, scalar=#'Value.Scalar'{value=Ports}},
                TaskInfo = #'TaskInfo'{
                                name = UniqueName,
                                task_id = #'TaskID'{ value = "task_id_" ++ UniqueName},
                                slave_id = Offer#'Offer'.slave_id,
                                resources = [MemoryResource, CpuResource],
                                command = #'CommandInfo'{value = Command}
                },
                lager:info("TaskInfo : ~p~n", [TaskInfo]),
                {ok,driver_running} = scheduler:launchTasks(Offer#'Offer'.id, [TaskInfo]),
                merkxx_request:close_request(Identifier)
    end,
  {noreply, State};
handle_cast({disconnect}, State) ->
  disconnect_from_mesos(),
  {noreply, State};
handle_cast({connect, MasterLocation}, State) ->
  register_with_mesos(MasterLocation),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  disconnect_from_mesos(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.