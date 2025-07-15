-module(time_tracking_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    io:format("Supervisor init starting...~n"),
    
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    io:format("Supervisor configured with strategy: one_for_one~n"),

    ChildSpecs = [
        #{
            id => time_tracking_db,
            start => {time_tracking_db, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [time_tracking_db]
        },
        #{
            id => time_tracking_rmq,
            start => {time_tracking_rmq, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [time_tracking_rmq]
        },
        #{
            id => time_tracking_server,
            start => {time_tracking_server, start_link, []},
            restart => permanent, 
            shutdown => 5000,
            type => worker,
            modules => [time_tracking_server]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
