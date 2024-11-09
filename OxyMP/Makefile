all: run

run:
	cargo run

trace:
	RUST_BACKTRACE=1 $(MAKE) run

check:
	for crate in $(shell find . -name 'Cargo.toml' -exec dirname {} \;); do \
		(echo "Checking $$crate" && cd $$crate && cargo clippy -- -Dwarnings); \
	done
