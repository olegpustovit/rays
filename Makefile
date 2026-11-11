PREFIX ?= $(HOME)/.local

.PHONY: build install

build:
	@test -e pxui/Cargo.toml || git submodule update --init
	cargo build --release -p rays_bin

install: build
	install -d $(PREFIX)/bin
	install target/release/rays $(PREFIX)/bin/rays
