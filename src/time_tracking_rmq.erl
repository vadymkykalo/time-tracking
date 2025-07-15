-module(time_tracking_rmq).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    connection :: pid(),
    channel :: pid(),
    config :: list()
}).

%% API functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    {ok, Host} = application:get_env(time_tracking, rabbitmq_host),
    {ok, Port} = application:get_env(time_tracking, rabbitmq_port),
    {ok, Username} = application:get_env(time_tracking, rabbitmq_username),
    {ok, Password} = application:get_env(time_tracking, rabbitmq_password),
    {ok, VHost} = application:get_env(time_tracking, rabbitmq_vhost),
    {ok, Queue} = application:get_env(time_tracking, rabbitmq_queue),
    
    io:format("Connecting to RabbitMQ at ~p:~p~n", [Host, Port]),
    
    % Convert string values to binary for RabbitMQ client
    UsernameBin = list_to_binary(Username),
    PasswordBin = list_to_binary(Password),
    VHostBin = list_to_binary(VHost),
    
    AmqpOpts = #amqp_params_network{
        host = Host,
        port = Port,
        username = UsernameBin,
        password = PasswordBin,
        virtual_host = VHostBin
    },
    
    process_flag(trap_exit, true),
    
    {ok, Connection} = amqp_connection:start(AmqpOpts),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    
    QueueDeclare = #'queue.declare'{
        queue = list_to_binary(Queue),
        durable = true,
        auto_delete = false
    },
    #'queue.declare_ok'{} = amqp_channel:call(Channel, QueueDeclare),
    
    % Set up consumer for RPC requests
    BasicConsume = #'basic.consume'{
        queue = list_to_binary(Queue),
        no_ack = false
    },
    
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, BasicConsume, self()),
    
    % Request handler
    spawn_link(fun() -> consume(Channel, Queue) end),
    
    {ok, #state{
        connection = Connection,
        channel = Channel,
        config = []
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

% Message consumer
consume(Channel, Queue) ->
    receive
        {#'basic.deliver'{delivery_tag = DeliveryTag, routing_key = RoutingKey}, 
         #amqp_msg{props = Props, payload = Payload}} ->
            
            % Process the request
            try
                % Decode JSON request
                {ok, Request} = time_tracking_json:decode(Payload),
                
                % Get request path from routing key
                Path = binary_to_list(RoutingKey),
                
                % Pass request to handler
                Response = time_tracking_server:handle_request(Path, Request),
                
                % Encode response to JSON
                {ok, ResponsePayload} = time_tracking_json:encode(Response),
                
                % Send response
                case Props#'P_basic'.reply_to of
                    undefined -> 
                        % No reply queue, can't respond
                        ok;
                    ReplyTo ->
                        ResponseProps = #'P_basic'{
                            correlation_id = Props#'P_basic'.correlation_id,
                            content_type = <<"application/json">>
                        },
                        
                        amqp_channel:cast(Channel, 
                            #'basic.publish'{
                                exchange = <<"">>,
                                routing_key = ReplyTo
                            },
                            #amqp_msg{props = ResponseProps, payload = ResponsePayload})
                end
            catch
                _:Error ->
                    % In case of error, return error information
                    error_logger:error_msg("Error processing RPC request: ~p~n", [Error])
            end,
            
            % Acknowledge message processing
            amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),
            
            % Recursively continue receiving messages
            consume(Channel, Queue);
            
        _Other ->
            % Ignore other messages
            consume(Channel, Queue)
    end.
