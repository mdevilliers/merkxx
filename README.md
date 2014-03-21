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
```
