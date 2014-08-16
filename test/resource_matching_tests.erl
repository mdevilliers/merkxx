-module (resource_matching_tests).

-include_lib("eunit/include/eunit.hrl").
-include ("merkxx.hrl").

singular_request_generous_offer_test()->

    Offer = #resource_offer{identifier="ABC",       memory=1000.0, cpu=5.0, disk=5000.0, ports=[30000, 40000]},
    Request = #provision_request{ identifier="123", memory=50.0,   cpu=1.0, disk=100.0,  ports=0, name="Name", start_command="Command" },

    [{match, [Allocated] }] = merkxx_resource_allocator:match([Offer], [Request]),

    ?assertEqual(Allocated#match.request_identifier, Request#provision_request.identifier),
    ?assertEqual(Allocated#match.offer_identifier, Offer#resource_offer.identifier).

singular_request_small_offer_test()->

    Offer = #resource_offer{identifier="ABC",       cpu=1.0,   memory=1000.0, disk=1000.0, ports=[30000, 40000]},
    Request = #provision_request{ identifier="123", cpu=100.0, memory=50.0,   disk=100.0,  ports=0, name="Name", start_command="Command" },

    [{no_matches}] = merkxx_resource_allocator:match([Offer], [Request]).

mutliple_request_generous_offer_test()->
    Offer = #resource_offer{identifier="ABC",memory=1000.0, cpu=50.0, disk=5000.0, ports=[30000, 40000]},
    Request =  #provision_request{ identifier="123", cpu=1.0, memory=50.0, ports=0, disk=100.0 ,name="Name1", start_command="Command1" },
    Request2 = #provision_request{ identifier="345", cpu=5.0, memory=250.0,ports=5, disk=500.0 ,name="Name2", start_command="Command2" },

    [{match, [Allocated1]}, {match, [Allocated2]}] = merkxx_resource_allocator:match([Offer], [Request,Request2]),

    ?assertEqual(Allocated1#match.request_identifier, Request#provision_request.identifier),
    ?assertEqual(Allocated1#match.offer_identifier, Offer#resource_offer.identifier),

    ?assertEqual(Allocated2#match.request_identifier, Request2#provision_request.identifier),
    ?assertEqual(Allocated2#match.offer_identifier, Offer#resource_offer.identifier).
