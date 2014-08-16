-module (merkxx).

-export ([start/0, run_command/2]).
-include ("merkxx.hrl").

start() ->
    lager:start(),
    ok = application:start(merkxx).

run_command(Name, Command) ->
    Request = #provision_request{ name=Name, start_command=Command },
    {ok, Identifier} = merkxx_request:new_request(Request),
    {ok, Identifier}.