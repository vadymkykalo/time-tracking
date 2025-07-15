#!/bin/bash
set -e

# Create log directory
mkdir -p /app/logs

# Wait for PostgreSQL
echo "Waiting for PostgreSQL..."
until PGPASSWORD=$POSTGRES_PASSWORD psql -h $POSTGRES_HOST -U $POSTGRES_USER -d $POSTGRES_DB -c '\q'; do
  >&2 echo "Postgres is unavailable - sleeping"
  sleep 1
done

echo "PostgreSQL is ready - initializing database"

# Initialize database from existing SQL file
PGPASSWORD=$POSTGRES_PASSWORD psql -h $POSTGRES_HOST -U $POSTGRES_USER -d $POSTGRES_DB -f /app/priv/init_db.sql

# Start the Erlang application
echo "Starting time_tracking application..."
cd /app && exec erl \
    -sname time_tracking \
    -pa /app/_build/default/lib/*/ebin \
    -config /app/config/sys \
    -eval "application:ensure_all_started(sasl), application:ensure_all_started(time_tracking)" \
    -noshell
