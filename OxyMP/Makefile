all: run

run:
	cargo run

trace:
	RUST_BACKTRACE=1 $(MAKE) run

check:
	cargo clippy -- -Dwarnings
	for crate in $(shell find . -name 'Cargo.toml' -exec dirname {} \;); do \
		(cd $$crate && cargo clippy -- -Dwarnings); \
	done
	
