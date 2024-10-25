# Specify the default Rust toolchain version here if needed
RUST_TOOLCHAIN=stable

.PHONY: rust-version format lint test all

# Display versions of all Rust tools
rust-version:
	@echo "Rust command-line utility versions:"
	@rustc --version                # Rust compiler
	@cargo --version                # Rust package manager
	@rustfmt --version || { echo "rustfmt not installed. Installing..."; rustup component add rustfmt; } # Rust code formatter
	@rustup --version               # Rust toolchain manager
	@clippy-driver --version || { echo "clippy not installed. Installing..."; rustup component add clippy; } # Rust linter

# Format all Rust code in the project
format:
	@echo "Formatting all projects with cargo fmt"
	cargo fmt --all

# Lint all Rust code with Clippy
lint:
	@echo "Linting all projects with Clippy"
	@rustup component add clippy 2> /dev/null # Ensure Clippy is installed
	cargo clippy --all-targets --all-features -- -D warnings

# Run tests for all projects
test:
	@echo "Running tests with cargo test"
	cargo test --all

# Run all steps: format, lint, and test
all: format lint test
	@echo "Completed format, lint, and test steps"
