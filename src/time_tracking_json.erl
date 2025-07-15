-module(time_tracking_json).

-export([encode/1, decode/1]).

encode(Term) ->
    try
        {ok, jiffy:encode(Term)}
    catch
        throw:Error ->
            {error, Error}
    end.

decode(JSON) ->
    try
        {ok, jiffy:decode(JSON, [return_maps])}
    catch
        throw:Error ->
            {error, Error}
    end.
