-module (merkxx).

-export ([start/0, connect/0, run_command/2]).
-include ("merkxx.hrl").

start() ->
    lager:start(),
    ok = application:start(merkxx).

connect() ->
    merkxx_mesos_worker:connect("127.0.1.1:5050").

run_command(Name, Command) ->
    Request = #provision_request{ name=Name, start_command=Command },
    {ok, Identifier} = merkxx_request:new_request(Request),
    {ok, Identifier}.