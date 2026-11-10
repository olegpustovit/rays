#!/bin/sh
# 256 MB main-thread stack on executables only: ld rejects -stack_size for dylibs (proc macros).
case " $* " in
*" -dynamiclib "*) exec cc "$@" ;;
*) exec cc "$@" -Wl,-stack_size,0x10000000 ;;
esac
