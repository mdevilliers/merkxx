-module (merkxx_mesos_worker).


-behaviour (scheduler).

-include_lib("erlang_mesos/include/mesos_pb.hrl").
-include ("merkxx.hrl").

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

resourceOffers( #'Offer'{id = OfferId, slave_id = SlaveId, resources = ResourceArr}, State) ->
   
    lager:info("Offer : ~p~n", [OfferId]),

    OfferedResources = examineResourcesInOffer(ResourceArr),

    lager:info("OfferedResources : ~p~n", [OfferedResources]),

    Cpu = proplists:get_value(cpu, OfferedResources),
    Memory = proplists:get_value(mem, OfferedResources),
    Ports = proplists:get_all_values(ports, OfferedResources),

    case merkxx_request:match_next_request({Cpu, Memory, Ports }) of
         {ok, []} ->
            lager:info("Declining offer", []),
            scheduler:declineOffer(OfferId);
         
         {ok, PendingRequests} ->
           lager:info("Pending requests ~p", [PendingRequests]),
            provision_requests(SlaveId,OfferId, PendingRequests)
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
examine_resource_offer([ #'Resource'{name = "ports", ranges = Ranges } | T ], Acc) when is_record(Ranges, 'Value.Ranges') ->
    examine_resource_offer(T, [ examine_ranges_offer(Ranges#'Value.Ranges'.range, Acc)|Acc]);
examine_resource_offer([ #'Resource'{name = Name, scalar = Scalar } | T ], Acc) when is_record(Scalar, 'Value.Scalar') ->
    examine_resource_offer(T, [{Name, Scalar#'Value.Scalar'.value}|Acc]).



examine_ranges_offer([], Acc) -> Acc ;
examine_ranges_offer([{'Value.Range',From,To} | T], Acc) ->
    examine_ranges_offer(T, [{ports, [From,To]}|Acc]).

provision_requests(_,_, []) -> ok;
provision_requests(SlaveId, OfferId, [ #provision_request{ type= <<"command">>, identifier=Identifier , name=Name, start_command=Command, cpu=RequestedCpu, memory=RequestedMemory } | T]) ->

        UniqueName = "Merkxx_Command_" ++ Name ++ "_" ++ Identifier,
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
        merkxx_request:close_request(Identifier),
        provision_requests(SlaveId, OfferId,T);
provision_requests(SlaveId, OfferId, [ #provision_request{ type= <<"docker">>, identifier=Identifier , name=Name, start_command=Command, cpu=RequestedCpu, memory=RequestedMemory, docker_image=DockerImage } | T]) ->

        UniqueName = "Merkxx_Docker_" ++ Name ++ "_" ++ Identifier,
        Scalar = mesos_pb:enum_symbol_by_value('Value.Type', 0),
        CpuResource = #'Resource'{name="cpus", type=Scalar, scalar=#'Value.Scalar'{value=RequestedCpu}},
        MemoryResource = #'Resource'{name="mem", type=Scalar, scalar=#'Value.Scalar'{value=RequestedMemory}},

        DockerInfo = #'ContainerInfo.DockerInfo'{ image = DockerImage},

        ContainerInfo = #'ContainerInfo'{ 
                docker = DockerInfo,
                type = 'DOCKER', 
                volumes = []},

        TaskInfo = #'TaskInfo'{
                        name = UniqueName,
                        task_id = #'TaskID'{ value = "task_id_" ++ UniqueName},
                        slave_id = SlaveId,
                        resources = [MemoryResource, CpuResource],
                        command = #'CommandInfo'{value = Command},
                        container = ContainerInfo
                        },

        lager:info("TaskInfo : ~p~n", [TaskInfo]),
        {ok,driver_running} = scheduler:launchTasks(OfferId, [TaskInfo]),
        merkxx_request:close_request(Identifier),
        provision_requests(SlaveId, OfferId,T).
