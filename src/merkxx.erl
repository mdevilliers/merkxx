-module (merkxx).

-export ([start/0, connect/0]).



start() ->
    lager:start(),
    ok = application:start(merkxx).

connect() ->
    merkxx_mesos_worker:connect("127.0.1.1:5050").

%provision_application() ->

