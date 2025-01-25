# Deployment Guide

## Components

The system consists of the following services managed by Supervisord:
- Redis server (port 6379)
- API server (port 8080)
- Writer service

## Using Supervisord (Development)

1. Install Supervisord:
   ```bash
   sudo apt-get update
   sudo apt-get install supervisor
   ```

2. Setup project:
   ```bash
   make setup
   make build
   ```

3. Start services:
   ```bash
   make start
   ```

## Monitoring

Check status:
```bash
make status
```

View logs:
```bash
make logs
```

### Log Locations
All logs are stored in the `logs` directory:
- Redis: `logs/redis.log`
- API: `logs/api.log`
- Writer: `logs/writer.log`
- Supervisord: `logs/supervisord.log`

## Common Commands

```bash
# Start all processes
make start

# Stop all processes
make stop

# View status
make status

# View logs
make logs

# Clean everything
make clean
```

## Web Interface (Optional)

To enable the web interface, add to supervisord.conf:
```ini
[inet_http_server]
port=*:9001
username=admin
password=your_password
```

Then access http://localhost:9001

## Process Control
Supervisord manages process priorities in the following order:
1. Redis (priority 10)
2. API Server (priority 50)
3. Writer Service (priority 100)

All services are configured to automatically restart on failure. 