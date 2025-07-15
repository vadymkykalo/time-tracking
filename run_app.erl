#!/usr/bin/env escript
%%! -sname debug -cookie time_tracking_cookie -pa /app/_build/default/lib/*/ebin -config /app/config/sys

main(_) ->
    io:format("Paths: ~p~n", [code:get_path()]),
    
    % Show loaded applications
    io:format("~nLoaded Applications:~n"),
    [io:format("~p: ~p~n", [App, Vsn]) || {App, _, Vsn} <- application:loaded_applications()],
    
    % Load time_tracking application with explicit path
    code:add_pathsa(["/app/_build/default/lib/time_tracking/ebin"]),
    io:format("~nLoading time_tracking application...~n"),
    case application:load(time_tracking) of
        ok -> io:format("Application loaded successfully~n");
        {error, {already_loaded, _}} -> io:format("Application already loaded~n");
        {error, Reason} -> io:format("Error loading application: ~p~n", [Reason])
    end,
    
    % Show environment
    io:format("~nApplication environment:~n~p~n", [application:get_all_env(time_tracking)]),
    
    % Start all dependencies first
    io:format("~nStarting dependencies...~n"),
    Apps = [crypto, ssl, inets, sasl, epgsql, amqp_client, jiffy],
    
    [start_app(App) || App <- Apps],
    
    % Start time_tracking
    io:format("~nStarting time_tracking...~n"),
    start_app(time_tracking),
    
    % Keep the script running
    io:format("~nApplication started, keeping VM alive...~n"),
    timer:sleep(infinity).

start_app(App) ->
    io:format("Starting ~p: ", [App]),
    case application:start(App) of
        ok -> 
            io:format("OK~n");
        {error, {already_started, _}} -> 
            io:format("Already started~n");
        {error, Reason} -> 
            io:format("ERROR: ~p~n", [Reason])
    end.
