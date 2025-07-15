-module(time_tracking).

-export([start/0, stop/0]).

start() ->
    application:ensure_all_started(time_tracking).

stop() ->
    application:stop(time_tracking).
