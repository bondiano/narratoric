# Narratoric

A simple game engine for narrative-driven games built with OCaml and Base.

## Quick Start

### Option 1: Automated Setup (Recommended)
```bash
./scripts/setup.sh
```

### Option 2: Manual Setup
1. Create opam switch and install dependencies:
```bash
make setup
```

2. Build the project:
```bash
make build
```

3. Run the example:
```bash
make run
```

## Requirements

- OCaml 5.3.0+
- opam 2.4.1+

## Development Commands

All commands are available via the Makefile:

```bash
make help      # Show all available commands
make setup     # First-time setup
make build     # Build the project
make run       # Run game engine demo
make test      # Run tests
make dev       # Development mode (watch files)
make clean     # Clean build artifacts
make format    # Format code
make install   # Install binary globally
```

### Core Features

- **Event System**: Process game events like scene changes, variable updates, player choices
- **State Management**: Track player variables, current scene, game progress
- **Immutable Architecture**: All operations return new state, no side effects
- **Type Safety**: Built with OCaml's strong type system using Base

### Event Types

- `SceneChange of string` - Switch to a new scene
- `VariableSet of string * string` - Set a game variable
- `PlayerChoice of int` - Record player choice
- `Game_start` / `Game_end` - Game lifecycle events

## Dependencies

- OCaml 5.3.0+
- Dune 3.17+
- Base (Jane Street's standard library replacement)
- Stdio (for I/O operations)

## CI/CD

This project uses GitHub Actions for continuous integration and deployment:

### Workflows

- **CI** (`ci.yml`): Runs on every push and pull request
  - Builds and tests on Ubuntu, macOS, and Windows
  - Checks code formatting with ocamlformat
  - Runs static analysis and linting
  - Generates documentation
  - Measures code coverage

- **Auto Format** (`format.yml`): Automatically formats code in pull requests
  - Runs ocamlformat on PR code
  - Commits formatting changes automatically
  - Comments on the PR when changes are made

- **Release** (`release.yml`): Triggered by version tags (e.g., `v1.0.0`)
  - Builds release binaries for Linux, macOS (Intel & ARM), and Windows
  - Creates GitHub release with artifacts
  - Prepares for opam package publication

- **Update Dependencies** (`deps.yml`): Weekly automated dependency updates
  - Runs every Monday
  - Updates all OCaml dependencies
  - Creates a pull request with changes

### Status Badges

Add these badges to show CI status:

```markdown
[![CI](https://github.com/bondiano/narratoric/actions/workflows/ci.yml/badge.svg)](https://github.com/bondiano/narratoric/actions/workflows/ci.yml)
[![Release](https://github.com/bondiano/narratoric/actions/workflows/release.yml/badge.svg)](https://github.com/bondiano/narratoric/actions/workflows/release.yml)
```
