
-record (provision_request,{ 
    identifier, 
    name, 
    start_command, 
    location, 
    status = <<"InProgress">> , 
    cpu = 1.0, 
    memory = 50.0,
    ports = 0,
    disk = 100.0}).

-record (resource_offer,{ 
    identifier, 
    cpu , 
    memory,
    ports,
    disk}).

-record (match,{ 
    request_identifier, 
    offer_identifier}).