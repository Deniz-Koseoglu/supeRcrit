#!/bin/bash
# Helper script to start Chrome in headless mode for shinytest2 tests
# Run this from Git Bash BEFORE running R tests on Windows
#
# Usage:
#   ./tests/start_chrome_for_tests.sh
#
# Then in R:
#   Sys.setenv(CHROMOTE_CHROME_DEBUG_PORT = "9515")
#   devtools::test(filter = "shinytest2")

PORT=${1:-9515}
USER_DATA_DIR="${TEMP:-/tmp}/chrome-shinytest2-${PORT}"

echo "Starting Chrome for shinytest2 tests..."
echo "Port: $PORT"
echo "User data dir: $USER_DATA_DIR"

# Create user data directory
mkdir -p "$USER_DATA_DIR"

# Find Chrome
CHROME_PATH=""
if [ -f "/c/Program Files/Google/Chrome/Application/chrome.exe" ]; then
    CHROME_PATH="/c/Program Files/Google/Chrome/Application/chrome.exe"
elif [ -f "/c/Program Files (x86)/Google/Chrome/Application/chrome.exe" ]; then
    CHROME_PATH="/c/Program Files (x86)/Google/Chrome/Application/chrome.exe"
else
    echo "ERROR: Chrome not found!"
    exit 1
fi

echo "Chrome path: $CHROME_PATH"

# Check if port is already in use
if curl -s "http://127.0.0.1:${PORT}/json/version" > /dev/null 2>&1; then
    echo "Chrome already running on port $PORT"
    curl -s "http://127.0.0.1:${PORT}/json/version"
    exit 0
fi

# Start Chrome in background
"$CHROME_PATH" \
    --headless=new \
    --remote-debugging-port=$PORT \
    --remote-allow-origins="http://127.0.0.1:${PORT}" \
    --user-data-dir="$USER_DATA_DIR" \
    --disable-gpu \
    --force-color-profile=srgb \
    --disable-extensions \
    --mute-audio \
    --no-first-run \
    --no-default-browser-check &

CHROME_PID=$!
echo "Chrome PID: $CHROME_PID"

# Wait for Chrome to start
for i in {1..10}; do
    sleep 1
    if curl -s "http://127.0.0.1:${PORT}/json/version" > /dev/null 2>&1; then
        echo ""
        echo "Chrome started successfully!"
        curl -s "http://127.0.0.1:${PORT}/json/version"
        echo ""
        echo "---"
        echo "Now run your R tests with:"
        echo "  Sys.setenv(CHROMOTE_CHROME_DEBUG_PORT = \"$PORT\")"
        echo "  devtools::test(filter = \"shinytest2\")"
        echo ""
        echo "To stop Chrome later, run:"
        echo "  kill $CHROME_PID"
        echo "  # or"
        echo "  curl http://127.0.0.1:${PORT}/json/close"
        exit 0
    fi
    echo "Waiting for Chrome to start... ($i/10)"
done

echo "ERROR: Chrome failed to start within 10 seconds"
exit 1
