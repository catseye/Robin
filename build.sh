#!/bin/sh

PROG=robin

if command -v ghc >/dev/null 2>&1; then
    echo "building $PROG.exe with ghc"
    (cd src && ghc --make Main.hs -o ../bin/$PROG.exe) || exit 1
else
    echo "ghc not found, not building $PROG.exe"
fi

./build-packages.sh
