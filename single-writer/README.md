# Single Writer Pattern Example

This project demonstrates the Single Writer Principle using Go, Redis, and SQLite.

## Architecture

- Votes are received by the API server and queued in Redis
- The single writer process continuously polls Redis for new votes
- When a vote is found, it:
  1. Updates the vote totals in Redis
  2. Saves the vote to SQLite
  3. Moves to the next vote

This ensures that only one process is writing to the database, preventing concurrency issues.

## API Endpoints

1. Submit a vote:
   ```bash
   curl -X POST http://localhost:8080/votes \
     -H "Content-Type: application/json" \
     -d '{"option": "option1"}'
   ```

2. Get vote totals:
   ```bash
   curl http://localhost:8080/votes
   ```
   Response:
   ```json
   {
     "option1": 42,
     "option2": 28
   }
   ```

3. Get a specific vote:
   ```bash
   curl http://localhost:8080/votes/123
   ```
   Response:
   ```json
   {
     "id": 123,
     "option": "option1",
     "created_at": "2024-03-20T15:04:05Z"
   }
   ```

## Prerequisites

1. Go 1.21 or later

## Quick Start

1. Install dependencies:
   ```bash
   make deps
   ```

2. Initial setup (creates required directories):
   ```bash
   make setup   # This will also run deps if not already run
   ```

3. Run database migrations:
   ```bash
   make migrate-up
   ```

4. Start all services:
   ```bash
   make start
   ```

## Available Make Commands

### Setup and Dependencies
- `make deps` - Install all required dependencies (supervisor, redis, golang-migrate)
- `make setup` - Create necessary directories (includes deps)

### Service Management
- `make start` - Start all services (API, writer, and Redis) using supervisor
- `make stop` - Stop all services
- `make status` - Check status of running services
- `make logs` - View service logs

### Development
- `make build` - Build API and writer binaries
- `make clean` - Clean up built files and logs

### Individual Services
- `make run-api` - Run API server directly (for development)
- `make run-writer` - Run writer process directly (for development)

### Database Migrations
- `make migrate-up` - Apply all pending migrations
- `make migrate-down` - Rollback last migration
- `make migrate-version` - Show current migration version
- `make migrate-new name=migration_name` - Create new migration files
- `make migrate-force version=000001` - Force set migration version (emergency use only)

## Database Migrations

This project uses `golang-migrate` to manage database schema changes. The migrations are stored in `db/migrations` directory.

### Migration Commands

1. Create a new migration:
   ```bash
   make migrate-new name=create_users_table
   ```

2. Apply all pending migrations:
   ```bash
   make migrate-up
   ```

3. Rollback the last migration:
   ```bash
   make migrate-down
   ```

4. Check current migration version:
   ```bash
   make migrate-version
   ```

5. Force set migration version (in case of emergency):
   ```bash
   make migrate-force version=000001
   ```

### Migration Files

- `db/migrations/000001_create_votes_table.up.sql`: Creates the initial schema
- `db/migrations/000001_create_votes_table.down.sql`: Reverts the initial schema 