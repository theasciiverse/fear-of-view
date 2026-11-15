#!/bin/sh
# Builds Fear of View's curses edition for the Asciiverse catalog into
# stage/: one static binary (fearOfView) that needs no Haskell, curses, gmp
# or other library on the player's machine, with the licence and readme.
#
# Needs GHC (9.8 or newer), cabal, and the static libraries of ncursesw,
# tinfo, gmp, ffi and numa (Debian and Ubuntu: libncurses-dev libgmp-dev
# libffi-dev libnuma-dev). The BearLibTerminal edition (-f bear) is left
# out: the catalog plays in the terminal.
#
# The binary links glibc statically too; it only warns about the user and
# group lookups the unix package carries, which the game never makes (it
# finds its folders through HOME and XDG_STATE_HOME). Terminfo comes from
# the player's system at run time, as for any curses program.
set -eu
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
FLAGS="-f curses -f -bear"
jobs=${JOBS:-4}
cabal build -j"$jobs" --builddir=dist-static $FLAGS \
  --ghc-options="-pgml $root/asciiverse/cc-static.sh -optl-static -optl-pthread" \
  exe:fearOfView
rm -rf stage
mkdir -p stage
cp "$(cabal list-bin --builddir=dist-static $FLAGS exe:fearOfView)" stage/
strip stage/fearOfView
cp COPYING README.md CHANGELOG.md stage/
file stage/fearOfView
ls -l stage
