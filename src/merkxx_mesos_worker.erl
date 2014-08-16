-module (merkxx_mesos_worker).


-behaviour (scheduler).

-include_lib("erlang_mesos/include/mesos_pb.hrl").

% api
-export ([exit/0]).    

% from scheduler
-export ([init/1, 
          registered/3, 
          reregistered/2, 
          disconnected/1, 
          offerRescinded/2, 
          statusUpdate/2, 
          frameworkMessage/4, 
          slaveLost/2, 
          executorLost/4, 
          error/2,
          resourceOffers/2]).

% api
init(MasterLocation) ->
    FrameworkInfo = #'FrameworkInfo'{user="", name="Merkxx"},
    State = [],
    {FrameworkInfo, MasterLocation, State}.

exit() ->
    {ok,driver_stopped} = scheduler:stop(0), % stop the scheduler
    ok = scheduler:destroy(). % destroy and cleanup the nif

% call backs
registered(FrameworkID, MasterInfo, State) ->
    io:format("Registered callback : ~p ~p~n", [FrameworkID, MasterInfo]),
    {ok,State}.

reregistered(MasterInfo, State) ->
    io:format("ReRegistered callback : ~p ~n", [MasterInfo]),
    {ok,State}.

resourceOffers({'Offer',{'OfferID',_} = OfferId,_,{'SlaveID',_} = SlaveId,_,ResourceArr,[],[]} = Offer, State) ->
    
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
    {ok,State}.

disconnected(State) ->
    io:format("Disconnected callback"),
    {ok,State}.

offerRescinded(OfferID, State) ->
    io:format("OfferRescinded callback : ~p ~n", [OfferID]),
    {ok,State}.

statusUpdate(StatusUpdate, State) ->
    io:format("StatusUpdate callback : ~p ~n", [StatusUpdate]),
    {ok,State}. 

frameworkMessage(ExecutorID, SlaveID, Message, State) ->
    io:format("FrameworkMessage callback : ~p ~p ~p ~n", [ExecutorID, SlaveID, Message]),
    {ok,State}.

slaveLost(SlaveID, State) ->
    io:format("SlaveLost callback : ~p ~n", [SlaveID]),
    {ok,State}.

executorLost(ExecutorID, SlaveID, Status, State) ->
    io:format("ExecutorLost callback : ~p ~p ~p ~n", [ExecutorID, SlaveID, Status]),
    {ok,State}.

error(Message, State) ->
    io:format("Error callback : ~p ~n", [Message]),
    {ok,State}.

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