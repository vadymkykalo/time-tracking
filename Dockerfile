FROM erlang:25.0

WORKDIR /app

# Copy rebar.config for dependency installation
COPY rebar.config ./

# Install dependencies
RUN rebar3 get-deps

# Copy application source code
COPY src/ ./src/
COPY config/ ./config/
COPY include/ ./include/
COPY test/ ./test/
COPY priv/ ./priv/

# Build application
RUN rebar3 compile

# Install postgresql client for healthcheck
RUN apt-get update && apt-get install -y postgresql-client && rm -rf /var/lib/apt/lists/*

# Copy entrypoint script
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

# Configure entry point
ENTRYPOINT ["/docker-entrypoint.sh"]
