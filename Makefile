.PHONY: help setup build clean test run install dev deps opam-files format lint

BINARY_NAME = narratoric

help:
	@echo "Narratoric - OCaml Game Engine"
	@echo ""
	@echo "Available commands:"
	@echo "  setup      - First time setup (creates opam switch and installs dependencies)"
	@echo "  deps       - Install/update dependencies"
	@echo "  build      - Build the project"
	@echo "  run        - Run the example narratoric"
	@echo "  test       - Run tests"
	@echo "  dev        - Development mode (watch and rebuild)"
	@echo "  clean      - Clean build artifacts"
	@echo "  format     - Format source code"
	@echo "  lint       - Run linter"
	@echo "  install    - Install binary globally"
	@echo ""

# First time setup
setup:
	@echo "Setting up development environment..."
	@if [ ! -d "_opam" ]; then \
		echo "Creating opam switch..."; \
		opam switch create . --deps-only; \
	fi
	@echo "Installing dependencies..."
	@eval `opam env` && opam install --deps-only .
	@echo "Generating opam files..."
	@eval `opam env` && dune build
	@echo "Setup complete! Run 'make build' to build the project."

# Install/update dependencies
deps: opam-files
	@echo "Installing/updating dependencies..."
	@eval `opam env` && opam install --deps-only .

# Generate opam files from dune-project
opam-files:
	@eval `opam env` && dune build @install

# Build the project (with format check)
build: opam-files format-check
	@echo "Building project..."
	@eval `opam env` && dune build

# Run the engine demo
run: build
	@echo "Running game engine demo..."
	@eval `opam env` && dune exec $(BINARY_NAME)

# Run tests
test: build
	@echo "Running tests..."
	@eval `opam env` && dune test

# Development mode with file watching
dev:
	@echo "Starting development mode (Ctrl+C to stop)..."
	@eval `opam env` && dune build --watch

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@dune clean
	@rm -f *.install

# Format source code
format:
	@echo "Formatting source code..."
	@eval `opam env` && dune build @fmt --auto-promote

# Check formatting (will fail if code is not formatted)
format-check:
	@echo "Checking code formatting..."
	@eval `opam env` && dune build @fmt

# Run linter (if ocamlformat is available)
lint:
	@echo "Running linter..."
	@eval `opam env` && dune build @check || echo "No linter configured"

# Install binary globally
install: build
	@echo "Installing $(BINARY_NAME) globally..."
	@eval `opam env` && dune install

# Check if opam switch exists
check-switch:
	@if [ ! -d "_opam" ]; then \
		echo "ERROR: No opam switch found. Run 'make setup' first."; \
		exit 1; \
	fi

# Override targets that need the switch to exist
build test run dev format lint install: check-switch