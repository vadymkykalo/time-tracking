-module(time_tracking_db).
-behaviour(gen_server).

%% API
-export([start_link/0, query/1, query/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    conn :: pid(),
    config :: list()
}).

%% API functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

query(Sql) ->
    gen_server:call(?SERVER, {query, Sql, []}).

query(Sql, Params) ->
    gen_server:call(?SERVER, {query, Sql, Params}).

%% gen_server callbacks
init([]) ->
    % Получаем настройки подключения к базе данных
    {ok, Host} = application:get_env(time_tracking, postgres_host),
    {ok, Port} = application:get_env(time_tracking, postgres_port),
    {ok, Username} = application:get_env(time_tracking, postgres_username),
    {ok, Password} = application:get_env(time_tracking, postgres_password),
    {ok, Database} = application:get_env(time_tracking, postgres_database),
    
    % Store complete config for reference
    Config = [
        {postgres_host, Host},
        {postgres_port, Port},
        {postgres_username, Username},
        {postgres_password, Password},
        {postgres_database, Database}
    ],
    
    io:format("Connecting to PostgreSQL at ~p:~p~n", [Host, Port]),
    
    % Формируем конфигурацию для epgsql
    ConnOpts = #{
        host => Host,
        port => Port,
        username => Username,
        password => Password,
        database => Database,
        timeout => 5000
    },
    
    case epgsql:connect(ConnOpts) of
        {ok, Conn} ->
            io:format("Successfully connected to PostgreSQL~n"),
            {ok, #state{conn = Conn, config = Config}};
        {error, Reason} ->
            io:format("Failed to connect to PostgreSQL: ~p~n", [Reason]),
            {stop, Reason}
    end.

handle_call({query, Sql, []}, _From, State = #state{conn = Conn}) ->
    Result = epgsql:squery(Conn, Sql),
    {reply, Result, State};

handle_call({query, Sql, Params}, _From, State = #state{conn = Conn}) ->
    Result = epgsql:equery(Conn, Sql, Params),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
