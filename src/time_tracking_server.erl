-module(time_tracking_server).
-behaviour(gen_server).

%% API
-export([start_link/0, handle_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    config :: list()
}).

%% API functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Processing incoming RPC requests
handle_request(Path, Request) ->
    gen_server:call(?SERVER, {handle, Path, Request}).

%% gen_server callbacks
init([]) ->
    Config = [
        {default_work_start_time, element(2, application:get_env(time_tracking, default_work_start_time))},
        {default_work_end_time, element(2, application:get_env(time_tracking, default_work_end_time))},
        {default_work_days, element(2, application:get_env(time_tracking, default_work_days))},
        {grace_period_minutes, element(2, application:get_env(time_tracking, grace_period_minutes))}
    ],
    
    io:format("Time tracking server initialized with config: ~p~n", [Config]),
    {ok, #state{config = Config}}.

handle_call({handle, Path, Request}, _From, State) ->
    Response = process_request(Path, Request),
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% Request routing
process_request("/card/touch", #{<<"card_uid">> := CardUID}) ->
    card_touch(CardUID);
process_request("/card/assign", #{<<"user_id">> := UserID, <<"card_uid">> := CardUID}) ->
    card_assign(UserID, CardUID);
process_request("/card/delete", #{<<"card_uid">> := CardUID}) ->
    card_delete(CardUID);
process_request("/card/list_by_user", #{<<"user_id">> := UserID}) ->
    card_list_by_user(UserID);
process_request("/card/delete_all_by_user", #{<<"user_id">> := UserID}) ->
    card_delete_all_by_user(UserID);
process_request("/work_time/set", #{<<"user_id">> := UserID, 
                                   <<"start_time">> := StartTime, 
                                   <<"end_time">> := EndTime, 
                                   <<"days">> := Days,
                                   <<"free_schedule">> := FreeSchedule}) ->
    work_time_set(UserID, StartTime, EndTime, Days, FreeSchedule);
process_request("/work_time/get", #{<<"user_id">> := UserID}) ->
    work_time_get(UserID);
process_request("/work_time/add_exclusion", #{<<"user_id">> := UserID, 
                                             <<"type_exclusion">> := TypeExclusion, 
                                             <<"start_datetime">> := StartDatetime, 
                                             <<"end_datetime">> := EndDatetime}) ->
    work_time_add_exclusion(UserID, TypeExclusion, StartDatetime, EndDatetime);
process_request("/work_time/get_exclusion", #{<<"user_id">> := UserID}) ->
    work_time_get_exclusion(UserID);
process_request("/work_time/history_by_user", #{<<"user_id">> := UserID}) ->
    work_time_history_by_user(UserID);
process_request("/work_time/history", #{<<"limit">> := Limit}) ->
    work_time_history(Limit);
process_request("/work_time/statistics_by_user", #{<<"user_id">> := UserID} = Request) ->
    % Optional parameter for period filter (week, month, year, all period)
    Period = maps:get(<<"period">>, Request, <<"month">>),
    work_time_statistics_by_user(UserID, Period);
process_request("/work_time/statistics", #{<<"limit">> := Limit}) ->
    work_time_statistics(Limit);
process_request(Path, _Request) ->
    #{error => <<"Unknown path: ", Path/binary>>}.

%% Request handlers

%% Cards

% Card touch registration in system
card_touch(CardUID) ->
    % Check if card exists in system
    case time_tracking_db:query("SELECT user_id FROM cards WHERE uid = $1", [CardUID]) of
        {ok, _, []} ->
            % Card not found, register event without user association
            #{card_uid => CardUID, user_id => null};
        {ok, _, [{UserID}]} ->
            % Card found, register employee arrival/departure
            % Check last record for this employee today
            {ok, _, LogType} = time_tracking_db:query(
                "SELECT log_type FROM time_logs WHERE user_id = $1 AND log_time::date = CURRENT_DATE ORDER BY log_time DESC LIMIT 1", 
                [UserID]),
            
            % Determine registration type (in or out)
            NewLogType = case LogType of
                [] -> "in";  % No records today - means this is arrival
                [{"in"}] -> "out";  % Last record - arrival, means now departure
                [{"out"}] -> "in"  % Last record - departure, means now arrival
            end,
            
            % Write event to log
            {ok, _, _} = time_tracking_db:query(
                "INSERT INTO time_logs (user_id, card_uid, log_type, log_time) VALUES ($1, $2, $3, CURRENT_TIMESTAMP)",
                [UserID, CardUID, NewLogType]),
            
            #{card_uid => CardUID, user_id => UserID, log_type => NewLogType};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Assign card to employee
card_assign(UserID, CardUID) ->
    % Check if user exists
    case time_tracking_db:query("SELECT id FROM users WHERE id = $1", [UserID]) of
        {ok, _, []} ->
            #{error => <<"User not found">>};
        {ok, _, _} ->
            % Check if card is already assigned to another user
            case time_tracking_db:query("SELECT user_id FROM cards WHERE uid = $1", [CardUID]) of
                {ok, _, []} ->
                    % Card not found, create new record
                    {ok, _, _} = time_tracking_db:query(
                        "INSERT INTO cards (uid, user_id) VALUES ($1, $2)",
                        [CardUID, UserID]),
                    #{card_uid => CardUID, user_id => UserID};
                {ok, _, [{ExistingUserID}]} when ExistingUserID == UserID ->
                    % Card already assigned to this user
                    #{card_uid => CardUID, user_id => UserID};
                {ok, _, [{_OtherUserID}]} ->
                    % Card assigned to another user, update
                    {ok, _, _} = time_tracking_db:query(
                        "UPDATE cards SET user_id = $1 WHERE uid = $2",
                        [UserID, CardUID]),
                    #{card_uid => CardUID, user_id => UserID};
                {error, Reason} ->
                    #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
            end;
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Delete card
card_delete(CardUID) ->
    case time_tracking_db:query("DELETE FROM cards WHERE uid = $1 RETURNING user_id", [CardUID]) of
        {ok, _, []} ->
            #{error => <<"Card not found">>};
        {ok, _, [{UserID}]} ->
            #{card_uid => CardUID, user_id => UserID};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Get user's cards list
card_list_by_user(UserID) ->
    case time_tracking_db:query("SELECT uid FROM cards WHERE user_id = $1", [UserID]) of
        {ok, _, Rows} ->
            CardUIDs = [UID || {UID} <- Rows],
            #{user_id => UserID, cards => CardUIDs};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Delete all user's cards
card_delete_all_by_user(UserID) ->
    case time_tracking_db:query("DELETE FROM cards WHERE user_id = $1 RETURNING uid", [UserID]) of
        {ok, _, Rows} ->
            CardUIDs = [UID || {UID} <- Rows],
            #{user_id => UserID, cards => CardUIDs};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

%% Work time

% Set employee work schedule
work_time_set(UserID, StartTime, EndTime, Days, FreeSchedule) ->
    % Convert days array to Postgres array format
    DaysStr = lists:flatten(io_lib:format("ARRAY[~s]", [string:join([integer_to_list(D) || D <- Days], ",")])),
    
    % Check if schedule already exists for this user
    case time_tracking_db:query("SELECT id FROM work_schedules WHERE user_id = $1", [UserID]) of
        {ok, _, []} ->
            % Create new schedule
            {ok, _, _} = time_tracking_db:query(
                "INSERT INTO work_schedules (user_id, start_time, end_time, days, free_schedule) VALUES ($1, $2, $3, " ++ DaysStr ++ ", $4)",
                [UserID, StartTime, EndTime, FreeSchedule]),
            #{user_id => UserID, start_time => StartTime, end_time => EndTime, days => Days, free_schedule => FreeSchedule};
        {ok, _, _} ->
            % Update existing schedule
            {ok, _, _} = time_tracking_db:query(
                "UPDATE work_schedules SET start_time = $1, end_time = $2, days = " ++ DaysStr ++ ", free_schedule = $3 WHERE user_id = $4",
                [StartTime, EndTime, FreeSchedule, UserID]),
            #{user_id => UserID, start_time => StartTime, end_time => EndTime, days => Days, free_schedule => FreeSchedule};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Get employee work schedule
work_time_get(UserID) ->
    case time_tracking_db:query("SELECT start_time, end_time, days, free_schedule FROM work_schedules WHERE user_id = $1", [UserID]) of
        {ok, _, []} ->
            #{error => <<"Work schedule not found for user">>};
        {ok, _, [{StartTime, EndTime, Days, FreeSchedule}]} ->
            % Convert PostgreSQL array to Erlang list
            DaysList = parse_pg_array(Days),
            #{user_id => UserID, start_time => StartTime, end_time => EndTime, days => DaysList, free_schedule => FreeSchedule};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Add exclusion to work schedule
work_time_add_exclusion(UserID, TypeExclusion, StartDatetime, EndDatetime) ->
    % Check exclusion type validity
    ValidTypes = ["late_arrival", "early_departure", "full_day_off"],
    case lists:member(TypeExclusion, ValidTypes) of
        true ->
            % Add exclusion
            {ok, _, _} = time_tracking_db:query(
                "INSERT INTO schedule_exclusions (user_id, type_exclusion, start_datetime, end_datetime) VALUES ($1, $2, $3, $4)",
                [UserID, TypeExclusion, StartDatetime, EndDatetime]),
            #{user_id => UserID, type_exclusion => TypeExclusion, start_datetime => StartDatetime, end_datetime => EndDatetime};
        false ->
            #{error => <<"Invalid exclusion type. Valid types are: late_arrival, early_departure, full_day_off">>}
    end.

% Get schedule exclusions
work_time_get_exclusion(UserID) ->
    case time_tracking_db:query(
        "SELECT id, type_exclusion, start_datetime, end_datetime FROM schedule_exclusions WHERE user_id = $1", 
        [UserID]) of
        {ok, _, Rows} ->
            Exclusions = [#{
                id => ID,
                type => Type,
                start_datetime => Start,
                end_datetime => End
            } || {ID, Type, Start, End} <- Rows],
            #{user_id => UserID, exclusions => Exclusions};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Get employee history
work_time_history_by_user(UserID) ->
    case time_tracking_db:query(
        "SELECT l.log_time, l.log_type, c.uid FROM time_logs l JOIN cards c ON l.card_uid = c.uid WHERE l.user_id = $1 ORDER BY l.log_time DESC", 
        [UserID]) of
        {ok, _, Rows} ->
            Logs = [#{
                time => Time,
                type => Type,
                card_uid => CardUID
            } || {Time, Type, CardUID} <- Rows],
            #{user_id => UserID, history => Logs};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Get all employees history
work_time_history(Limit) ->
    case time_tracking_db:query(
        "SELECT u.id, u.name, l.log_time, l.log_type, c.uid FROM time_logs l JOIN cards c ON l.card_uid = c.uid JOIN users u ON l.user_id = u.id ORDER BY l.log_time DESC LIMIT $1", 
        [Limit]) of
        {ok, _, Rows} ->
            Logs = [#{
                user_id => UserID,
                user_name => UserName,
                time => Time,
                type => Type,
                card_uid => CardUID
            } || {UserID, UserName, Time, Type, CardUID} <- Rows],
            #{history => Logs};
        {error, Reason} ->
            #{error => list_to_binary(io_lib:format("Database error: ~p", [Reason]))}
    end.

% Get employee statistics
work_time_statistics_by_user(UserID, Period) ->
    % Define start date based on selected filter
    {StartDateQuery, PeriodName} = case Period of
        <<"week">> -> {"DATE_TRUNC('week', CURRENT_DATE)", "week"};
        <<"month">> -> {"DATE_TRUNC('month', CURRENT_DATE)", "month"};
        <<"year">> -> {"DATE_TRUNC('year', CURRENT_DATE)", "year"};
        <<"all">> -> {"'1970-01-01'::date", "all time"}
    end,
    
    % Get user's schedule
    {ok, _, ScheduleRows} = time_tracking_db:query(
        "SELECT start_time, end_time, days, free_schedule FROM work_schedules WHERE user_id = $1", 
        [UserID]),
    
    case ScheduleRows of
        [] ->
            #{error => <<"Work schedule not found for user">>};
        [{StartTime, EndTime, Days, FreeSchedule}] ->
            % Convert PostgreSQL array to Erlang list
            DaysList = parse_pg_array(Days),
            
            % Get required work time in seconds
            {ok, _, [{TotalRequiredTime}]} = time_tracking_db:query(
                "SELECT SUM(CASE WHEN EXTRACT(DOW FROM day) = ANY(" ++ Days ++ ") THEN 1 ELSE 0 END) * 
                 EXTRACT(EPOCH FROM ($1::time - $2::time)) AS required_seconds 
                 FROM generate_series(" ++ StartDateQuery ++ ", CURRENT_DATE, '1 day'::interval) day",
                [EndTime, StartTime]),
            
            % Get actual worked time
            {ok, _, [{TotalWorkedTime}]} = time_tracking_db:query(
                "SELECT COALESCE(SUM(
                    EXTRACT(EPOCH FROM 
                      CASE WHEN log_type = 'out' THEN log_time 
                           ELSE NULL END - 
                      CASE WHEN log_type = 'in' THEN log_time 
                           ELSE NULL END
                    )
                  ), 0) AS worked_seconds
                  FROM (
                    SELECT user_id, log_type, log_time,
                           ROW_NUMBER() OVER (PARTITION BY user_id, DATE(log_time) ORDER BY log_time) as rn
                    FROM time_logs 
                    WHERE user_id = $1
                    AND log_time >= " ++ StartDateQuery ++ "
                  ) AS numbered_logs
                  WHERE (log_type = 'in' AND rn % 2 = 1) OR (log_type = 'out' AND rn % 2 = 0)",
                [UserID]),
            
            % Get number of late arrivals
            {ok, _, [{LateArrivalsWithReason, LateArrivalsWithoutReason}]} = time_tracking_db:query(
                "SELECT 
                    COUNT(CASE WHEN e.type_exclusion = 'late_arrival' THEN 1 END) as with_reason,
                    COUNT(CASE WHEN e.id IS NULL THEN 1 END) as without_reason
                 FROM (
                     SELECT l.log_time, se.id, se.type_exclusion
                     FROM time_logs l 
                     LEFT JOIN schedule_exclusions se ON 
                         l.user_id = se.user_id AND 
                         se.type_exclusion = 'late_arrival' AND
                         l.log_time BETWEEN se.start_datetime AND se.end_datetime
                     WHERE l.user_id = $1 AND 
                           l.log_type = 'in' AND
                           l.log_time >= " ++ StartDateQuery ++ " AND
                           EXTRACT(HOUR FROM l.log_time) * 60 + EXTRACT(MINUTE FROM l.log_time) > 
                           EXTRACT(HOUR FROM $2::time) * 60 + EXTRACT(MINUTE FROM $2::time)
                 ) e",
                [UserID, StartTime]),
            
            % Get number of early departures
            {ok, _, [{EarlyDeparturesWithReason, EarlyDeparturesWithoutReason}]} = time_tracking_db:query(
                "SELECT 
                    COUNT(CASE WHEN e.type_exclusion = 'early_departure' THEN 1 END) as with_reason,
                    COUNT(CASE WHEN e.id IS NULL THEN 1 END) as without_reason
                 FROM (
                     SELECT l.log_time, se.id, se.type_exclusion
                     FROM time_logs l 
                     LEFT JOIN schedule_exclusions se ON 
                         l.user_id = se.user_id AND 
                         se.type_exclusion = 'early_departure' AND
                         l.log_time BETWEEN se.start_datetime AND se.end_datetime
                     WHERE l.user_id = $1 AND 
                           l.log_type = 'out' AND
                           l.log_time >= " ++ StartDateQuery ++ " AND
                           EXTRACT(HOUR FROM l.log_time) * 60 + EXTRACT(MINUTE FROM l.log_time) < 
                           EXTRACT(HOUR FROM $2::time) * 60 + EXTRACT(MINUTE FROM $2::time)
                 ) e",
                [UserID, EndTime]),
                
            % Calculate shortage of work time
            ShortageSeconds = list_to_integer(TotalRequiredTime) - list_to_integer(TotalWorkedTime),
            
            % Format result
            #{
                user_id => UserID,
                period => PeriodName,
                schedule => #{
                    start_time => StartTime,
                    end_time => EndTime,
                    work_days => DaysList,
                    free_schedule => FreeSchedule
                },
                required_work_time => format_seconds(list_to_integer(TotalRequiredTime)),
                actual_work_time => format_seconds(list_to_integer(TotalWorkedTime)),
                shortage => format_seconds(max(0, ShortageSeconds)),
                late_arrivals => #{
                    with_reason => list_to_integer(LateArrivalsWithReason),
                    without_reason => list_to_integer(LateArrivalsWithoutReason)
                },
                early_departures => #{
                    with_reason => list_to_integer(EarlyDeparturesWithReason),
                    without_reason => list_to_integer(EarlyDeparturesWithoutReason)
                }
            }
    end.

% Get general statistics for all employees
work_time_statistics(Limit) ->
    % Get statistics for each user
    {ok, _, Users} = time_tracking_db:query(
        "SELECT id, name FROM users ORDER BY id LIMIT $1",
        [Limit]),
    
    UserStats = lists:map(
        fun({UserID, UserName}) ->
            Stats = work_time_statistics_by_user(UserID, <<"month">>),
            Stats#{user_name => UserName}
        end,
        Users
    ),
    
    #{statistics => UserStats}.

%% Helper functions

% Convert Postgres array to Erlang list
parse_pg_array(ArrayStr) ->
    % Remove curly braces and split into elements
    TrimmedStr = string:trim(ArrayStr, both, "{}"),
    Elements = string:tokens(TrimmedStr, ","),
    [list_to_integer(Element) || Element <- Elements].

% Format seconds to readable format
format_seconds(TotalSeconds) ->
    Hours = TotalSeconds div 3600,
    Minutes = (TotalSeconds rem 3600) div 60,
    Seconds = TotalSeconds rem 60,
    io_lib:format("~2..0B:~2..0B:~2..0B", [Hours, Minutes, Seconds]).
