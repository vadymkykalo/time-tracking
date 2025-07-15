#!/bin/bash

# Run the debug script and capture all output
cd /app && /app/debug_start.erl > /app/logs/debug_output.log 2>&1
RESULT=$?

# Print the output regardless of success or failure
echo "Debug script completed with exit code $RESULT"
echo "Debug log output:"
cat /app/logs/debug_output.log

# Keep container running for inspection
echo "Keeping container alive for inspection..."
tail -f /dev/null
