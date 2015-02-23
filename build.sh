#!/bin/sh
set -e
git clean -xdff
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal build
cabal test
cabal check
