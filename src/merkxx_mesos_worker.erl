-module (merkxx_mesos_worker).

-behaviour (gen_server).
-behaviour (scheduler).

-include_lib("erlang_mesos/include/mesos_pb.hrl").

-export ([start_link/0]).

% gen server
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
registered(MesosState, FrameworkID, MasterInfo) ->
    lager:info("Mesos::Registered callback : ~p ~p~n", [FrameworkID, MasterInfo]),
    {ok,MesosState}.

reregistered(MesosState, MasterInfo) ->
    lager:info("Mesos::ReRegistered callback : ~p ~n", [MasterInfo]),
    {ok,MesosState}.

resourceOffers(MesosState, Offer) ->
    lager:info("Mesos::ResourceOffers callback : ~p ~n", [Offer]),
    gen_server:cast(?MODULE, {resourceOffers, Offer}),
    {ok,MesosState}.

disconnected(MesosState) ->
    lager:info("Mesos::Disconnected callback"),
    {ok,MesosState}.

offerRescinded(MesosState, OfferID) ->
    lager:info("Mesos::OfferRescinded callback : ~p ~n", [OfferID]),
    {ok,MesosState}.

statusUpdate(MesosState, StatusUpdate) ->
    lager:info("Mesos::StatusUpdate callback : ~p ~n", [StatusUpdate]),
    {ok,MesosState}. 

frameworkMessage(MesosState, ExecutorID, SlaveID, Message) ->
    lager:info("Mesos::FrameworkMessage callback : ~p ~p ~p ~n", [ExecutorID, SlaveID, Message]),
    {ok,MesosState}.

slaveLost(MesosState, SlaveID) ->
    lager:info("Mesos::SlaveLost callback : ~p ~n", [SlaveID]),
    {ok,MesosState}.

executorLost(MesosState, ExecutorID, SlaveID, Status) ->
    lager:info("Mesos::ExecutorLost callback : ~p ~p ~p ~n", [ExecutorID, SlaveID, Status]),
    {ok,MesosState}.

error(MesosState, Message) ->
    lager:error("Mesos::Error callback : ~p ~n", [Message]),
    {ok,MesosState}.

%% gen server API
init([]) ->
    lager:info("merkxx_mesos_worker started.", []),
    {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({resourceOffers, {'Offer',{'OfferID',_} = OfferId,_,{'SlaveID',_} = SlaveId,_,ResourceArr,[],[]}} = Offer, State) ->
  
    lager:info("Offer : ~p~n", [Offer]),

    OfferedResources = examineResourcesInOffer(ResourceArr),

    Cpu = proplists:get_value(cpu, OfferedResources),
    Memory = proplists:get_value(mem, OfferedResources),
    Ports = proplists:get_value(ports, OfferedResources),

    case merkxx_request:match_next_request({Cpu, Memory, Ports }) of
         {ok, []} ->
            lager:info("Declining offer", []),
            scheduler:declineOffer(OfferId);
         
         {ok, [ {provision_request, Identifier , Name, Command, _, _ , RequestedCpu , RequestedMemory , _,_ } ]} ->
            
                UniqueName = "Merkxx_" ++ Name ++ "_" ++ Identifier,
                Scalar = mesos_pb:enum_symbol_by_value('Value.Type', 0),
                CpuResource = #'Resource'{name="cpus", type=Scalar, scalar=#'Value.Scalar'{value=RequestedCpu}},
                MemoryResource = #'Resource'{name="mem", type=Scalar, scalar=#'Value.Scalar'{value=RequestedMemory}},
                TaskInfo = #'TaskInfo'{
                                name = UniqueName,
                                task_id = #'TaskID'{ value = "task_id_" ++ UniqueName},
                                slave_id = SlaveId,
                                resources = [MemoryResource, CpuResource],
                                command = #'CommandInfo'{value = Command}
                                },

                lager:info("TaskInfo : ~p~n", [TaskInfo]),
                {ok,driver_running} = scheduler:launchTasks(OfferId, [TaskInfo]),
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


examineResourcesInOffer(ResourceArr) ->
    examine_resource_offer(ResourceArr, []).

examine_resource_offer([], Acc) ->
    Acc;
examine_resource_offer([{'Resource',"cpus",'SCALAR',{'Value.Scalar',CPU},_,_,_}|T], Acc) ->
    examine_resource_offer(T, [{cpu, CPU}|Acc]);
examine_resource_offer([{'Resource',"mem",'SCALAR',{'Value.Scalar',Mem},_,_,_}|T], Acc) ->
    examine_resource_offer(T, [{mem, Mem}|Acc]);
examine_resource_offer([{'Resource',"disk",'SCALAR',{'Value.Scalar',Disk},_,_,_}|T], Acc) ->
    examine_resource_offer(T, [{disk, Disk}|Acc]);
examine_resource_offer([{'Resource',"ports",'RANGES',_,{'Value.Ranges',[{'Value.Range',From,To}]},_,_}|T], Acc) ->
    examine_resource_offer(T, [{ports, [From,To]}|Acc]).