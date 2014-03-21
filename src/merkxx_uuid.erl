-module (merkxx_uuid).

-export([generate/0]).

generate() ->
   uuid:uuid_to_string(uuid:get_v4(weak), binary_standard).
