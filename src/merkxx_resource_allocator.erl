-module (merkxx_resource_allocator).


-export ([match/2]).
-include ("merkxx.hrl").

match(Offers,Requests) ->
    lists:map( fun (R) -> iterate_offers(Offers,R, []) end, Requests).

iterate_offers([],_,[]) ->
    {no_matches};
iterate_offers([],_, MatchedOffers) ->
    {match, MatchedOffers };
iterate_offers([H|T],Request, MatchedOffers) ->

    % to do port matching
    case (H#resource_offer.cpu  >= Request#provision_request.cpu) and
         (H#resource_offer.memory  >= Request#provision_request.memory ) and
         (H#resource_offer.disk  >= Request#provision_request.disk ) of
        false  ->
            iterate_offers(T,Request,MatchedOffers);
        true -> 
            Match = #match{ request_identifier = Request#provision_request.identifier, 
                            offer_identifier =  H#resource_offer.identifier},
            iterate_offers(T,Request,[Match | MatchedOffers])
    end.