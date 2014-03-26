-module (resource_matching_tests).

-include_lib("eunit/include/eunit.hrl").
-include ("merkxx.hrl").

singular_request_generous_offer_test()->

    Offer = #resource_offer{identifier="ABC", memory=1000.0, cpu=5.0, disk=5000.0, ports=[30000, 40000]},
    Request = #provision_request{ identifier="123", name="Name", start_command="Command", cpu=1.0, memory=50.0, ports=0, disk=100.0 },

    [{match, [Allocated] }] = merkxx_resource_alllocator:match([Offer], [Request]),

    ?assertEqual(Allocated#match.request_identifier, Request#provision_request.identifier),
    ?assertEqual(Allocated#match.offer_identifier, Offer#resource_offer.identifier).

singular_request_small_offer_test()->

    Offer = #resource_offer{identifier="ABC", memory=1000.0, cpu=1.0, disk=1000.0, ports=[30000, 40000]},
    Request = #provision_request{ identifier="123", name="Name", start_command="Command", cpu=100.0, memory=50.0, ports=0, disk=100.0 },

    [{no_matches}] = merkxx_resource_alllocator:match([Offer], [Request]).

mutliple_request_generous_offer_test()->
    Offer = #resource_offer{identifier="ABC",memory=1000.0, cpu=50.0, disk=5000.0, ports=[30000, 40000]},
    Request =  #provision_request{ identifier="123", name="Name1", start_command="Command1", cpu=1.0, memory=50.0, ports=0, disk=100.0 },
    Request2 = #provision_request{ identifier="345", name="Name2", start_command="Command2", cpu=5.0, memory=250.0, ports=5, disk=500.0 },

    {match, [Allocated1 | _Allocated2] } = merkxx_resource_alllocator:match([Offer], [Request,Request2]),
    %to do check Allocated2
    ?assertEqual(Allocated1#match.request_identifier, Request#provision_request.identifier),
    ?assertEqual(Allocated1#match.offer_identifier, Offer#resource_offer.identifier).

%[
%{match, request_identifier=123, offer_identifier="ABC"}
%{match, request_identifier=345, offer_identifier="ABC"}
%]