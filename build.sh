#!/bin/sh

for PROG in robinri whitecap; do
    if command -v ghc >/dev/null 2>&1; then
        echo "building $PROG.exe with ghc"
        ghc -isrc --make src/mains/$PROG/Main.lhs -o bin/$PROG.exe || exit 1
    else
        echo "ghc not found, not building $PROG.exe"
    fi
done

./build-packages.sh
