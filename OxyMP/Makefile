all: run

run:
	cargo run

trace:
	RUST_BACKTRACE=1 $(MAKE) run

check:
	for crate in $(shell find . -name 'Cargo.toml' -exec dirname {} \;); do \
		(echo "Checking $$crate" && cd $$crate && cargo clippy -- -Dwarnings); \
	done

expand:
	@FILE_NAME=src/test/$(shell date +"%Y-%m-%d-%H-%M").rs && \
	cargo expand > $$FILE_NAME && \
	bat $$FILE_NAME
