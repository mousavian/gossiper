-module(gossiper_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gossiper:start_link().

stop(_State) ->
    ok.
