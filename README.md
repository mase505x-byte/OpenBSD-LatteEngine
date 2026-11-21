# OpenBSD-LatteEngine
OpenBSD-LatteEngine is an experimental compiler/runtime project. It is not yet
a complete standalone game: the repository contains an assembler experiment,
runtime implementations, and an unfinished Latte compiler.

## OpenBSD build

Install the available build tools:

```sh
doas pkg_add ghc
```

Clang-based `cc`, `ar`, and `make` are included with OpenBSD. From the repository
root, build the C runtime:

```sh
make
```

This creates `lib/runtime.o`. To run the Haskell assembler check:

```sh
make check
```

Both commands work with OpenBSD's default `make` and compiler.

## Manual C build

The runtime can also be compiled and archived directly:

```sh
mkdir -p lib
cc -O2 -std=c11 -Wall -Wextra -Wpedantic -c altruntime.c -o lib/runtime.o
ar rcs lib/libruntime.a lib/runtime.o
```

## Current entry points

- `altruntime.c`: the C runtime library.
- `src/Assembler.hs`: the buildable Haskell assembler/encoding module.
- `src/test.lat`: a sample Latte program, not yet linked to a complete compiler.
- `runtime-gmae-bsd.hs`: an experimental Haskell runtime module.
- `prime-runtime.c`, `runtime.c`: drafts; neither is the working C runtime.

The SDL/game example in `src/test.lat` needs SDL2 and a working Latte compiler
front end before it can be built:

```sh
doas pkg_add sdl2
```
