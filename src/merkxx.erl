-module (merkxx).

-export ([start/0, connect/0, provision_application/3]).
-include ("merkxx.hrl").

start() ->
    lager:start(),
    ok = application:start(merkxx).

connect() ->
    merkxx_mesos_worker:connect("127.0.1.1:5050").

provision_application(Name, Command, Location) ->
    Request = #provision_request{ name=Name, start_command=Command, location=Location },
    {ok, Identifier} = merkxx_request:new_request(Request),
    {ok, Identifier}.