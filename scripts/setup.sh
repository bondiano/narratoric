#!/bin/bash

set -e

echo "🎮 Narrative Engine Setup Script"
echo "================================="

# Check if opam is installed
if ! command -v opam &> /dev/null; then
    echo "❌ Error: opam is not installed."
    echo "Please install opam first:"
    echo "  - macOS: brew install opam"
    echo "  - Ubuntu: apt-get install opam"
    echo "  - Other: https://opam.ocaml.org/doc/Install.html"
    exit 1
fi

echo "✅ opam found"

# Initialize opam if needed
if [ ! -d "$HOME/.opam" ]; then
    echo "🔧 Initializing opam..."
    opam init --auto-setup -y
fi

# Check if we're in the project directory
if [ ! -f "dune-project" ]; then
    echo "❌ Error: Please run this script from the narrative-engine project directory"
    exit 1
fi

echo "🏗️  Setting up project environment..."

# Create local opam switch if it doesn't exist
if [ ! -d "_opam" ]; then
    echo "📦 Creating local opam switch..."
    opam switch create . ocaml-base-compiler.5.1.0 --deps-only -y
    eval $(opam env)
else
    echo "✅ Local opam switch already exists"
    eval $(opam env)
fi

# No additional system dependencies needed for basic game engine

# Generate opam files
echo "📄 Generating opam files..."
dune build

# Install OCaml dependencies
echo "📚 Installing OCaml dependencies..."
opam install --deps-only . -y

# Build the project
echo "🔨 Building project..."
dune build

# Run tests
echo "🧪 Running tests..."
dune test

echo ""
echo "✅ Setup completed successfully!"
echo ""
echo "Next steps:"
echo "  - Run 'make run' to see the game engine demo"
echo "  - Run 'make help' to see all available commands"
echo "  - Check lib/engine.ml to see the game engine API"
echo ""
echo "Happy coding! 🚀"