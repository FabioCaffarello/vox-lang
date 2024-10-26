#!/bin/bash

echo "Starting setup for vox-lang..."

# Ensure Rust is installed
if ! command -v rustup &> /dev/null; then
    echo "Rust is not installed. Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
else
    echo "Rust is already installed."
fi

# Install nightly toolchain if not installed
if ! rustup show | grep -q 'nightly'; then
    echo "Installing Rust nightly toolchain..."
    rustup install nightly
else
    echo "Rust nightly toolchain is already installed."
fi

# Ensure necessary Rust components are installed for both stable and nightly
echo "Installing required Rust components..."
rustup component add rustfmt clippy
rustup component add rustfmt clippy --toolchain nightly

# Install cargo-tarpaulin for coverage reporting
if ! command -v cargo-tarpaulin &> /dev/null; then
    echo "Installing cargo-tarpaulin for code coverage..."
    cargo install cargo-tarpaulin || {
        echo "Error: cargo-tarpaulin installation failed. Please check dependencies."
        exit 1
    }
else
    echo "cargo-tarpaulin is already installed."
fi

# Install cargo-husky for Git hooks
if ! grep -q '[dev-dependencies.cargo-husky]' Cargo.toml; then
    echo "Adding cargo-husky to dev dependencies in Cargo.toml..."
    cargo add cargo-husky --dev
fi

# Set up hooks if in a Git repository
if [ -d ".git" ]; then
    echo "Configuring Git to use .cargo-husky/hooks for hooks..."
    chmod +x .cargo-husky/hooks/pre-commit
    chmod +x .cargo-husky/hooks/commit-msg
    git config core.hooksPath .cargo-husky/hooks
else
    echo "Warning: Not in a Git repository. Skipping Git hook configuration."
fi

echo "Setup complete! You can now start using the project."
