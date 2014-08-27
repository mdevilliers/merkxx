About
-----

Application to play with erlang-mesos binding. 
Once started you can send commands to messos to run.

Run
---

```
./rebar get-deps
./rebar compile

./rebar compile skip_deps=true
erl -pa ebin deps/*/ebin -s merkxx

merkxx:run_command("MyCommandName", "while sleep 10; do date -u +%T; done"). 

merkxx:run_docker_container("MyDockerApplication", "libmesos/ubuntu", "while sleep 10; do date -u +%T; done"). 

```

Create release
--------------

```
./relx
```


