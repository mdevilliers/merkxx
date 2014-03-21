
-record (provision_request,{ 
    identifier, 
    name, 
    start_command, 
    location, 
    status = <<"InProgress">> , 
    cpu = 1, 
    memory = 512,
    ports = 0 }).
