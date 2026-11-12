# Rays always loads libpxui (the PXUI stable C ABI) at run time. Cargo cannot order a cdylib before a dependent's link, so `lib`
# builds it first (into target/<profile>, where the binaries' rpath finds it); `cargo t` does the same. A plain `cargo build -p rays_bin`
# works once `make lib` (PROFILE=debug for the debug profile) has run.
PREFIX ?= $(HOME)/.local
APPDIR ?= /Applications
PROFILE ?= release
CODESIGN_IDENTITY ?=
CARGOFLAGS = $(if $(filter release,$(PROFILE)),--release)
OS := $(shell uname -s)
ifeq ($(OS),Darwin)
LIBSO = libpxui.dylib
LIBSONAME = libpxui.1.dylib
else
LIBSO = libpxui.so
LIBSONAME = libpxui.so.1
endif

.PHONY: build lib install

lib:
	@test -e pxui/Cargo.toml || git submodule update --init
	cargo build $(CARGOFLAGS) --manifest-path pxui/Cargo.toml -p pxui_dylib --target-dir target
	ln -sfn $(LIBSO) target/$(if $(filter release,$(PROFILE)),release,debug)/$(LIBSONAME)

build: lib
	cargo build $(CARGOFLAGS) -p rays_bin

# macOS: a signed Rays.app in $(APPDIR) plus a `rays` command in $(PREFIX)/bin. Elsewhere: rays and libpxui under $(PREFIX)
# (the binary's rpath is $$ORIGIN/../lib; Linux only, untested).
install: PROFILE = release
install: build
	install -d $(PREFIX)/bin
ifeq ($(OS),Darwin)
	CODESIGN_IDENTITY='$(CODESIGN_IDENTITY)' packaging/bundle-macos.sh '$(APPDIR)'
	ln -sfn '$(APPDIR)/Rays.app/Contents/MacOS/rays' $(PREFIX)/bin/rays
else
	install -d $(PREFIX)/lib
	install target/release/rays $(PREFIX)/bin/rays
	install -m 755 target/release/libpxui.so $(PREFIX)/lib/libpxui.so.1.0
	ln -sfn libpxui.so.1.0 $(PREFIX)/lib/libpxui.so.1
endif
