-module(time_tracking_client).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([call/2]).

%% API for testing RPC calls
call(Path, Request) ->
    % Get configuration
    {ok, Config} = application:get_env(time_tracking),
    Host = proplists:get_value(rabbitmq_host, Config),
    Port = proplists:get_value(rabbitmq_port, Config),
    Username = proplists:get_value(rabbitmq_username, Config),
    Password = proplists:get_value(rabbitmq_password, Config),
    VHost = proplists:get_value(rabbitmq_vhost, Config),
    Queue = proplists:get_value(rabbitmq_queue, Config),
    
    % Connect to RabbitMQ
    AmqpOpts = #amqp_params_network{
        host = Host,
        port = Port,
        username = Username,
        password = Password,
        virtual_host = VHost
    },
    
    {ok, Connection} = amqp_connection:start(AmqpOpts),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    
    % Create temporary queue for response
    #'queue.declare_ok'{queue = ReplyTo} = 
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),
    
    % Generate correlation ID
    CorrelationId = list_to_binary(integer_to_list(rand:uniform(100000))),
    
    % Convert request to JSON
    {ok, Payload} = time_tracking_json:encode(Request),
    
    % Send request
    Props = #'P_basic'{
        reply_to = ReplyTo,
        correlation_id = CorrelationId,
        content_type = <<"application/json">>
    },
    
    PublishMethod = #'basic.publish'{
        exchange = <<"">>,
        routing_key = list_to_binary([Queue, ".", Path]),
        mandatory = true
    },
    
    amqp_channel:register_return_handler(Channel, self()),
    
    amqp_channel:cast(Channel, PublishMethod, #amqp_msg{
        props = Props,
        payload = Payload
    }),
    
    % Set up consumer for response
    amqp_channel:subscribe(Channel, #'basic.consume'{
        queue = ReplyTo,
        no_ack = true
    }, self()),
    
    receive
        #'basic.consume_ok'{} -> ok
    end,
    
    % Wait for response
    Response = receive
        {#'basic.deliver'{}, #amqp_msg{props = #'P_basic'{correlation_id = CorrelationId}, 
                                       payload = ResponsePayload}} ->
            {ok, DecodedResponse} = time_tracking_json:decode(ResponsePayload),
            DecodedResponse;
        {#'basic.return'{}, _} ->
            #{error => <<"Request returned: queue not found">>}
    after 5000 ->
        #{error => <<"Timeout waiting for response">>}
    end,
    
    % Close connection
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    
    Response.
