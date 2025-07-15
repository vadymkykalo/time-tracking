#!/usr/bin/env escript
%%! -sname debug

main(_) ->
    % Show loaded applications
    io:format("==== Loaded Applications ====~n"),
    [io:format("~p: ~p~n", [App, Vsn]) || {App, _, Vsn} <- application:loaded_applications()],
    
    % Start dependencies one by one
    io:format("~n==== Starting Dependencies ====~n"),
    
    Apps = [crypto, asn1, public_key, ssl, inets, sasl, rabbit_common, amqp_client, epgsql, jiffy],
    
    [start_app(App) || App <- Apps],
    
    % Start time_tracking
    io:format("~n==== Starting time_tracking ====~n"),
    case application:start(time_tracking) of
        ok ->
            io:format("time_tracking started successfully~n"),
            % Keep the script running
            timer:sleep(infinity);
        {error, Reason} ->
            io:format("Failed to start time_tracking: ~p~n", [Reason]),
            init:stop(1)
    end.

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
