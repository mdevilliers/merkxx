
-record (provision_request,{ 
    identifier, 
    name, 
    type,
    start_command,
    docker_image, 
    location, 
    status = <<"InProgress">> , 
    cpu = 1.0, 
    memory = 5.0,
    ports = 0,
    disk = 10.0}).
