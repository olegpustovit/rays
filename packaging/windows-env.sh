# The Windows build environment: opam switch rays-win over MSYS2 mingw64, whose gcc built the
# compiler (ucrt64 libraries must not reach it). Source it from MSYS2's bash, never Git Bash's: two
# msys runtimes on one PATH lose environment variables. .envrc sources it for direnv.
rays_opam="$(cygpath -F 28)/opam/rays-win" # 28: the local application data folder
export OPAMSWITCH=rays-win
export TEMP="$(cygpath -w /tmp)" TMP="$(cygpath -w /tmp)" # without one, dune writes its temporary files into the tree
export OPAM_SWITCH_PREFIX="$(cygpath -w "$rays_opam")"
export CAML_LD_LIBRARY_PATH="$OPAM_SWITCH_PREFIX\\lib\\stublibs;$OPAM_SWITCH_PREFIX\\lib\\ocaml\\stublibs;$OPAM_SWITCH_PREFIX\\lib\\ocaml"
export MSYSTEM=MINGW64 MSYSTEM_PREFIX=/mingw64 MSYSTEM_CARCH=x86_64 MSYSTEM_CHOST=x86_64-w64-mingw32
export MINGW_CHOST=x86_64-w64-mingw32 MINGW_PREFIX=/mingw64 MINGW_PACKAGE_PREFIX=mingw-w64-x86_64
export PKG_CONFIG_PATH='C:\msys64\mingw64\lib\pkgconfig;C:\msys64\mingw64\share\pkgconfig'
export PATH="$rays_opam/bin:/mingw64/bin:/usr/bin:$PATH"
unset rays_opam
