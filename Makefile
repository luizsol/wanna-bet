# Makefile
# This Makefile is intended to help execute the main server-related tasks

# Goals
.DEFAULT_GOAL := dev

dev:
	@echo "Starting the development environment..."
	@mkdir -p data
	@docker-compose up -d
	@lein figwheel

down:
	@echo "Full stop!"
	@docker-compose down
