-module(time_tracking_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Log the startup with configuration
    io:format("Starting time_tracking application with configuration: ~p~n", 
              [application:get_all_env(time_tracking)]),
    
    % Start the supervisor first
    case time_tracking_sup:start_link() of
        {ok, Pid} ->
            io:format("Supervisor started successfully~n"),
            % Give processes a moment to initialize
            timer:sleep(1000),
            {ok, Pid};
        Error ->
            io:format("Failed to start supervisor: ~p~n", [Error]),
            Error
    end.

stop(_State) ->
    ok.
