About
-----

Application to play with erlang-mesos binding.


Run
---

```
./rebar get-deps
./rebar compile

./rebar compile skip_deps=true
erl -pa ebin deps/*/ebin

merkxx:start().
merkxx:connect().


merkxx:provision_application("MyApplicationName", "sleep 50", "LocationNotUsedAtTheMoment"). 
```
