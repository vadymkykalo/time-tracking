#!/usr/bin/env escript
%%! -sname debug -cookie time_tracking_cookie

main(_) ->
    % Print process info
    io:format("Process info: ~p~n", [erlang:process_info(self())]),
    
    % Show all modules
    io:format("~nLoaded Modules:~n"),
    [io:format("~p~n", [M]) || M <- code:all_loaded()],
    
    % Show loaded applications
    io:format("~nLoaded Applications:~n"),
    [io:format("~p: ~p~n", [App, Vsn]) || {App, _, Vsn} <- application:loaded_applications()],
    
    % Load time_tracking application
    io:format("~nLoading time_tracking application...~n"),
    case application:load(time_tracking) of
        ok -> io:format("Application loaded successfully~n");
        {error, {already_loaded, _}} -> io:format("Application already loaded~n");
        {error, Reason} -> io:format("Error loading application: ~p~n", [Reason])
    end,
    
    % Show environment
    io:format("~nApplication environment:~n~p~n", [application:get_all_env(time_tracking)]),
    
    % Start just the minimal parts
    try
        % Load basic applications
        application:start(sasl),
        
        % Try to start time_tracking
        io:format("~nStarting time_tracking...~n"),
        case application:start(time_tracking) of
            ok ->
                io:format("Application started successfully!~n"),
                timer:sleep(10000),
                ok;
            {error, Reason2} ->
                io:format("Failed to start: ~p~n", [Reason2]),
                % Show loaded applications again
                io:format("~nLoaded Applications after failure:~n"),
                [io:format("~p: ~p~n", [App, Vsn]) || {App, _, Vsn} <- application:loaded_applications()],
                % Show running applications
                io:format("~nRunning Applications:~n"),
                [io:format("~p: ~p~n", [App, Vsn]) || {App, _, Vsn} <- application:which_applications()]
        end
    catch
        E:R:S ->
            io:format("Exception: ~p:~p~nStacktrace: ~p~n", [E, R, S])
    end,
    
    % Keep script running for a while
    io:format("~nDebug script completed~n").
