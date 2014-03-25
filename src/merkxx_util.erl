-module (merkxx_util).

-export([generate_uuid/0]).
-export ([publish/2, subscribe/1]).

generate_uuid() ->
   uuid:uuid_to_string(uuid:get_v4(weak), binary_standard).

publish(EventType, Msg) ->
    Key = {EventType},
    gproc:send({p, l, Key}, { self(), Key, Msg }).

subscribe(Event) ->
    gproc:reg({p, l, { Event }}).
