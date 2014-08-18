-module(merkxx_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	MasterLocation = get_value(mesos_url, "127.0.1.1:5050"),
    merkxx_sup:start_link(MasterLocation).

stop(_State) ->
    ok.

get_value(Key, Default) ->
  case application:get_env(Key) of
        {ok, Value} -> Value;
        undefined -> Default
  end.
