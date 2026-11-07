PREFIX ?= $(HOME)/.local

.PHONY: build install

build:
	cargo build --release -p rays_bin

install: build
	install -d $(PREFIX)/bin
	install target/release/rays $(PREFIX)/bin/rays
