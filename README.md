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
erl -pa ebin deps/*/ebin

merkxx:start().

merkxx:run_command("MyCommandName", "sleep 50"). 
```
