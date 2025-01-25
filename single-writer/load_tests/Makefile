.PHONY: install run clean venv

# Variables
VENV_NAME=.venv
PYTHON=$(VENV_NAME)/bin/python
PIP=$(VENV_NAME)/bin/pip

# Default number of users and spawn rate
USERS=20
SPAWN_RATE=2
HOST=http://localhost:8080
LOG_LEVEL?=INFO

# Create virtual environment and install dependencies
venv:
	python3 -m venv $(VENV_NAME)
	$(PIP) install --upgrade pip
	$(PIP) install -r requirements.txt

# Install dependencies
install: requirements.txt
	$(PIP) install -r requirements.txt

# Run Locust with web interface
run:
	LOG_LEVEL=$(LOG_LEVEL) $(PYTHON) -m locust -f locustfile.py --host $(HOST)

# Run Locust headless mode with specified number of users
run-headless:
	LOG_LEVEL=$(LOG_LEVEL) $(PYTHON) -m locust -f locustfile.py --host $(HOST) --headless -u $(USERS) -r $(SPAWN_RATE) --run-time 5m

# Run Locust with HTML report
run-report:
	LOG_LEVEL=$(LOG_LEVEL) $(PYTHON) -m locust -f locustfile.py --host $(HOST) --headless -u $(USERS) -r $(SPAWN_RATE) --run-time 5m --html report.html

# Clean up
clean:
	rm -rf $(VENV_NAME)
	rm -f report.html
	find . -type d -name "__pycache__" -exec rm -r {} +

# Help
help:
	@echo "Available commands:"
	@echo "  make venv        - Create virtual environment and install dependencies"
	@echo "  make install     - Install dependencies in existing venv"
	@echo "  make run         - Run Locust with web interface"
	@echo "  make run-headless- Run Locust in headless mode (USERS=20 SPAWN_RATE=2)"
	@echo "  make run-report  - Run Locust and generate HTML report"
	@echo "  make clean       - Clean up virtual environment and reports"
	@echo ""
	@echo "You can override default values:"
	@echo "  make run-headless USERS=100 SPAWN_RATE=10 HOST=http://myapi.com LOG_LEVEL=DEBUG" 