-module(time_tracking_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for microservice main functionality

%% ===== Card handling tests =====
card_touch_test() ->
    % Mock database query for new card
    meck:new(time_tracking_db),
    meck:expect(time_tracking_db, query, 
        fun("SELECT user_id FROM cards WHERE uid = $1", [<<"card123">>]) -> 
            {ok, [], []} 
        end),
    
    Result = time_tracking_server:process_request("/card/touch", #{<<"card_uid">> => <<"card123">>}),
    meck:unload(time_tracking_db),
    
    ?assertEqual(#{card_uid => <<"card123">>, user_id => null}, Result).

card_touch_with_existing_card_test() ->
    % Mock database query for existing card
    meck:new(time_tracking_db),
    meck:expect(time_tracking_db, query, 
        fun("SELECT user_id FROM cards WHERE uid = $1", [<<"card456">>]) -> 
            {ok, [], [{1}]} 
        end),
    meck:expect(time_tracking_db, query, 
        fun("SELECT log_type FROM time_logs WHERE user_id = $1 AND log_time::date = CURRENT_DATE ORDER BY log_time DESC LIMIT 1", [1]) -> 
            {ok, [], []} 
        end),
    meck:expect(time_tracking_db, query, 
        fun("INSERT INTO time_logs (user_id, card_uid, log_type, log_time) VALUES ($1, $2, $3, CURRENT_TIMESTAMP)",
            [1, <<"card456">>, "in"]) -> 
            {ok, [], []} 
        end),
    
    Result = time_tracking_server:process_request("/card/touch", #{<<"card_uid">> => <<"card456">>}),
    meck:unload(time_tracking_db),
    
    ?assertEqual(#{card_uid => <<"card456">>, user_id => 1, log_type => "in"}, Result).

card_assign_test() ->
    % Mock database queries for card assignment
    meck:new(time_tracking_db),
    meck:expect(time_tracking_db, query, 
        fun("SELECT id FROM users WHERE id = $1", [1]) -> 
            {ok, [], [{1}]} 
        end),
    meck:expect(time_tracking_db, query, 
        fun("SELECT user_id FROM cards WHERE uid = $1", [<<"card789">>]) -> 
            {ok, [], []} 
        end),
    meck:expect(time_tracking_db, query, 
        fun("INSERT INTO cards (uid, user_id) VALUES ($1, $2)", [<<"card789">>, 1]) -> 
            {ok, [], []} 
        end),
    
    Result = time_tracking_server:process_request("/card/assign", #{<<"user_id">> => 1, <<"card_uid">> => <<"card789">>}),
    meck:unload(time_tracking_db),
    
    ?assertEqual(#{card_uid => <<"card789">>, user_id => 1}, Result).

%% ===== Work time tests =====
work_time_set_test() ->
    % Mock database queries for setting work time
    meck:new(time_tracking_db),
    meck:expect(time_tracking_db, query, 
        fun("SELECT id FROM work_schedules WHERE user_id = $1", [1]) -> 
            {ok, [], []} 
        end),
    meck:expect(time_tracking_db, query, 
        fun(Query, _) when is_list(Query) -> 
            {ok, [], []} 
        end),
    
    Result = time_tracking_server:process_request("/work_time/set", #{
        <<"user_id">> => 1,
        <<"start_time">> => <<"09:00:00">>,
        <<"end_time">> => <<"18:00:00">>,
        <<"days">> => [1, 2, 3, 4, 5],
        <<"free_schedule">> => false
    }),
    meck:unload(time_tracking_db),
    
    ?assertEqual(#{
        user_id => 1,
        start_time => <<"09:00:00">>,
        end_time => <<"18:00:00">>,
        days => [1, 2, 3, 4, 5],
        free_schedule => false
    }, Result).

work_time_add_exclusion_test() ->
    % Mock database queries for adding exclusion
    meck:new(time_tracking_db),
    meck:expect(time_tracking_db, query, 
        fun("INSERT INTO schedule_exclusions (user_id, type_exclusion, start_datetime, end_datetime) VALUES ($1, $2, $3, $4)",
            [1, "late_arrival", <<"2025-07-15 10:00:00">>, <<"2025-07-15 18:00:00">>]) -> 
            {ok, [], []} 
        end),
    
    Result = time_tracking_server:process_request("/work_time/add_exclusion", #{
        <<"user_id">> => 1,
        <<"type_exclusion">> => <<"late_arrival">>,
        <<"start_datetime">> => <<"2025-07-15 10:00:00">>,
        <<"end_datetime">> => <<"2025-07-15 18:00:00">>
    }),
    meck:unload(time_tracking_db),
    
    ?assertEqual(#{
        user_id => 1,
        type_exclusion => <<"late_arrival">>,
        start_datetime => <<"2025-07-15 10:00:00">>,
        end_datetime => <<"2025-07-15 18:00:00">>
    }, Result).

%% ===== Statistics tests =====
parse_pg_array_test() ->
    % Test PostgreSQL array parsing function
    ?assertEqual([1, 2, 3, 4, 5], time_tracking_server:parse_pg_array("{1,2,3,4,5}")).

format_seconds_test() ->
    % Test seconds formatting function
    ?assertEqual("02:30:45", time_tracking_server:format_seconds(9045)).
