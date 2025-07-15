#!/bin/bash

# Script for starting the time tracking microservice

# Check PostgreSQL connection
echo "Checking PostgreSQL connection..."
until pg_isready -h ${POSTGRES_HOST:-localhost} -p ${POSTGRES_PORT:-5432} -U ${POSTGRES_USER:-postgres}
do
  echo "Waiting for PostgreSQL to start..."
  sleep 1
done
echo "PostgreSQL is running"

# Check RabbitMQ connection
echo "Checking RabbitMQ connection..."
until timeout 5 bash -c "cat < /dev/null > /dev/tcp/${RABBITMQ_HOST:-localhost}/${RABBITMQ_PORT:-5672}"
do
  echo "Waiting for RabbitMQ to start..."
  sleep 1
done
echo "RabbitMQ is running"

# Configure from environment variables
if [ ! -z "$POSTGRES_HOST" ]; then
  sed -i "s/localhost/$POSTGRES_HOST/g" config/sys.config
fi

if [ ! -z "$RABBITMQ_HOST" ]; then
  sed -i "s/rabbitmq_host, \"localhost\"/rabbitmq_host, \"$RABBITMQ_HOST\"/g" config/sys.config
fi

echo "Starting application..."
rebar3 shell
