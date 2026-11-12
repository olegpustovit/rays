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
else ifneq (,$(findstring NT-,$(OS)))
# MSYS2 (uname -s is MINGW64_NT-...): pxui.dll and its import library land in target/<profile>; no soname link, the dll sits beside rays.exe.
WINDOWS = 1
else
LIBSO = libpxui.so
LIBSONAME = libpxui.so.1
endif

.PHONY: build lib install

lib:
	@test -e pxui/Cargo.toml || git submodule update --init
	cargo build $(CARGOFLAGS) --manifest-path pxui/Cargo.toml -p pxui_dylib --target-dir target
ifndef WINDOWS
	ln -sfn $(LIBSO) target/$(if $(filter release,$(PROFILE)),release,debug)/$(LIBSONAME)
endif

build: lib
	cargo build $(CARGOFLAGS) -p rays_bin

# macOS: a signed Rays.app in $(APPDIR) plus a `rays` command in $(PREFIX)/bin. Linux: rays and libpxui under $(PREFIX) (the binary's
# rpath is $$ORIGIN/../lib; SDL3 stays a system library). Windows (MSYS2): rays.exe, pxui.dll and the DLLs they load from the MSYS2
# prefix (SDL3, its runtime) together in $(PREFIX)/bin, which runs from anywhere.
install: PROFILE = release
install: build
	install -d $(PREFIX)/bin
ifeq ($(OS),Darwin)
	CODESIGN_IDENTITY='$(CODESIGN_IDENTITY)' packaging/bundle-macos.sh '$(APPDIR)'
	ln -sfn '$(APPDIR)/Rays.app/Contents/MacOS/rays' $(PREFIX)/bin/rays
else ifdef WINDOWS
	install target/release/rays.exe target/release/pxui.dll $(PREFIX)/bin
	ldd target/release/rays.exe | awk '/\/(ucrt64|mingw64|clang64)\//{print $$3}' | xargs -I{} install {} $(PREFIX)/bin
else
	install -d $(PREFIX)/lib
	install target/release/rays $(PREFIX)/bin/rays
# lld puts every -L directory holding a shared library (target/release, the SDL3 prefix) into RUNPATH: overwrite it in place with the
# installed layout's, NUL padded to the same length.
	perl -0pi -e 's{\$$ORIGIN:\$$ORIGIN/\.\.:\$$ORIGIN/\.\./lib:[^\0]*}{q($$ORIGIN/../lib) . "\0" x (length($$&) - 14)}e' $(PREFIX)/bin/rays
	install -m 755 target/release/libpxui.so $(PREFIX)/lib/libpxui.so.1.0
	ln -sfn libpxui.so.1.0 $(PREFIX)/lib/libpxui.so.1
endif
