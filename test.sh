#!/bin/sh

./build.sh || exit 1

TESTDOCS1="
doc/Robin.md
doc/Intrinsics.md
doc/Reactor.md
"

if [ "${FIXTURE}x" = "x" ]; then
    FIXTURE=fixture/whitecap.md
fi
echo "Using fixture $FIXTURE..."

echo "Running tests on core semantics..."
falderal -b $FIXTURE $TESTDOCS1 || exit 1

for PACKAGE in small intrinsics-wrappers fun boolean arith list env misc; do
    echo "Running tests on '$PACKAGE' package..."
    falderal -b $FIXTURE pkg/$PACKAGE.robin || exit 1
done

rm -f config.md
