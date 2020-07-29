#!/bin/sh

if [ "${NOBUILD}x" = "x" ]; then
  ./build.sh || exit 1
fi

if [ "${APPLIANCES}x" = "x" ]; then
  APPLIANCES="appliances/robin.md appliances/robin-no-builtins.md"
fi

echo "Running tests on core semantics..."
falderal -b $APPLIANCES doc/Robin.md || exit 1

if [ "${PACKAGES}x" = "x" ]; then
  PACKAGES="intrinsics small boolean arith list env misc"
fi

for PACKAGE in $PACKAGES; do
    echo "Running tests on '$PACKAGE' package..."
    falderal -b $APPLIANCES pkg/$PACKAGE.robin || exit 1
done

if [ "x$FORCE_HUGS" != "x" ] ; then
    runhugs -isrc src/QuickCheckTests.hs || exit 1
elif command -v runhaskell 2>&1 >/dev/null ; then
    runhaskell -isrc src/QuickCheckTests.hs || exit 1
fi
