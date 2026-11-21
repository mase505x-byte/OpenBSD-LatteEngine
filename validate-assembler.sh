#!/bin/sh
set -eu

if ! command -v ghc >/dev/null 2>&1; then
  sudo -A apt-get update
  sudo -A apt-get install -y ghc
fi

ghc -fno-code -Wall Assembler.hs
