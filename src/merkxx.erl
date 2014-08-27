-module (merkxx).

-export ([start/0, run_command/2,run_docker_container/3]).
-include ("merkxx.hrl").

start() ->
    lager:start(),
    ok = application:start(merkxx).

run_command(Name, Command) ->
    Request = #provision_request{ name=Name, start_command=Command, type= <<"command">> },
    {ok, Identifier} = merkxx_request:new_request(Request),
    {ok, Identifier}.

run_docker_container(Name, ImageUri, Command) ->
	Request = #provision_request{ name=Name, start_command=Command, type= <<"docker">>, docker_image=ImageUri },
    {ok, Identifier} = merkxx_request:new_request(Request),
    {ok, Identifier}.