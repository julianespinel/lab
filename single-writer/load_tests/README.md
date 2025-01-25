# Load Testing Suite

This folder contains load testing scripts for the voting application using Locust.

## Test Scenarios

The load test simulates users performing the following flow:
1. Cast a vote for a random option
2. Check vote totals
3. Verify their own vote

Each user waits 1-3 seconds between actions to simulate realistic behavior.

## Prerequisites

- Python 3.8 or higher
- Make

## Setup

1. Create a virtual environment and install dependencies:
```bash
make venv
```

## Available Commands

- `make run` - Start Locust with web interface (http://localhost:8089)
- `make run-headless` - Run Locust in headless mode with default settings
- `make run-report` - Run Locust and generate HTML report
- `make clean` - Clean up virtual environment and generated files

## Configuration

You can override default values when running headless tests:

```bash
make run-headless USERS=100 SPAWN_RATE=10 HOST=http://localhost:8080
```

Default values:
- USERS: 20
- SPAWN_RATE: 2
- HOST: http://localhost:8080

## Viewing Results

1. Web Interface (Interactive mode):
   - Start with `make run`
   - Open http://localhost:8089
   - Configure users and spawn rate
   - Start the test
   - View real-time metrics and charts

2. HTML Report (Headless mode):
   - Run `make run-report`
   - Open the generated `report.html` file

## Logging

The test suite includes comprehensive logging to help understand test execution and troubleshoot issues:

### Log Levels

- **DEBUG**: Detailed information about each request attempt
- **INFO**: Successful operations, test progress, and timing information
- **ERROR**: Failed requests and flow execution errors

### What's Logged

1. **Test Lifecycle Events**
   - Test start and stop
   - User creation and termination
   - Number of votes cast per user

2. **Request Details**
   - Pre-request intentions
   - Response status and content
   - Vote IDs and options selected
   - Current vote totals

3. **Performance Metrics**
   - Flow duration for each user
   - Success/failure status
   - Response times

4. **Error Information**
   - HTTP status codes
   - Full error responses
   - Validation failures

### Configuring Log Level

You can adjust the logging verbosity by setting the LOG_LEVEL environment variable:

```bash
# For most detailed output
LOG_LEVEL=DEBUG make run

# For standard information (default)
make run

# For errors only
LOG_LEVEL=ERROR make run
```
