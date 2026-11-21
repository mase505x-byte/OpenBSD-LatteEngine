CC ?= cc
CFLAGS ?= -O2 -std=c11 -Wall -Wextra -Wpedantic
AR ?= ar

.PHONY: all runtime clean check

all: runtime

runtime: lib/runtime.o

lib/runtime.o: altruntime.c
	mkdir -p lib
	$(CC) $(CFLAGS) -c altruntime.c -o lib/runtime.o

lib/libruntime.a: lib/runtime.o
	$(AR) rcs lib/libruntime.a lib/runtime.o

check: lib/runtime.o
	cd src && ghc -fno-code -Wall Assembler.hs
	ghc -fno-code -Wall runtime-gmae-bsd.hs

clean:
	rm -f lib/runtime.o lib/libruntime.a
