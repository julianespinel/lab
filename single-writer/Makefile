.PHONY: all build run-api run-writer clean setup start stop status logs migrate-new migrate-up migrate-down migrate-version migrate-force deps

# Install dependencies
deps:
	@echo "Installing dependencies..."
	@echo "Installing Go dependencies..."
	go mod tidy
	@echo "Installing golang-migrate..."
	go install -tags 'sqlite3' github.com/golang-migrate/migrate/v4/cmd/migrate@latest
	@echo "Checking/Installing supervisor..."
	which supervisord || (echo "Installing supervisor..." && sudo apt-get update && sudo apt-get install -y supervisor)
	@echo "Checking/Installing redis..."
	which redis-server || (echo "Installing redis..." && sudo apt-get update && sudo apt-get install -y redis-server)

# Create necessary directories
setup: deps
	@echo "Creating necessary directories..."
	mkdir -p bin logs run data

# Build both binaries
build: setup
	@echo "Building API server..."
	go build -o bin/api cmd/api/main.go
	@echo "Building writer process..."
	go build -o bin/writer cmd/writer/main.go

# Run the API server
run-api: build
	@echo "Starting API server..."
	./bin/api

# Run the writer process
run-writer: build
	@echo "Starting writer process..."
	./bin/writer

# Clean built binaries and logs
clean:
	@echo "Cleaning up build artifacts and logs..."
	rm -rf bin/ logs/ run/ data/
	rm -f votes.db
	rm -f /tmp/supervisor.${USER}.sock

# Start supervisor
start: build stop
	@echo "Starting all services using supervisor..."
	supervisord -c deploy/supervisord.conf

# Stop supervisor
stop:
	@echo "Stopping all services..."
	supervisorctl -c deploy/supervisord.conf shutdown || true
	rm -f /tmp/supervisor.${USER}.sock

# Restart all services
restart: stop start
	@echo "Services have been restarted"

# Show status of running services
status:
	@echo "Checking service status..."
	supervisorctl -c deploy/supervisord.conf status

# View logs
logs:
	@echo "Tailing service logs..."
	tail -f logs/*.log

# Database Migration Commands
.PHONY: migrate-new migrate-up migrate-down migrate-version migrate-force

# Create a new migration file
migrate-new:
	@if [ -z "$(name)" ]; then \
		echo "Error: name is required. Usage: make migrate-new name=create_users_table"; \
		exit 1; \
	fi
	@echo "Creating new migration '$(name)'..."
	$(HOME)/go/bin/migrate create -ext sql -dir db/migrations -seq $(name)

# Run all pending migrations
migrate-up:
	@echo "Running all pending migrations..."
	$(HOME)/go/bin/migrate -database "sqlite3://./votes.db" -path db/migrations up

# Rollback the last migration
migrate-down:
	@echo "Rolling back the last migration..."
	$(HOME)/go/bin/migrate -database "sqlite3://./votes.db" -path db/migrations down 1

# Show current migration version
migrate-version:
	@echo "Checking current migration version..."
	$(HOME)/go/bin/migrate -database "sqlite3://./votes.db" -path db/migrations version

# Force set migration version
migrate-force:
	@if [ -z "$(version)" ]; then \
		echo "Error: version is required. Usage: make migrate-force version=000001"; \
		exit 1; \
	fi
	@echo "Force setting migration version to $(version)..."
	$(HOME)/go/bin/migrate -database "sqlite3://./votes.db" -path db/migrations force $(version)
