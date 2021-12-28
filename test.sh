#!/bin/sh

if [ "${NOBUILD}x" = "x" ]; then
  ./build-packages.sh || exit 1
fi

if [ "${APPLIANCES}x" = "x" ]; then
  APPLIANCES="appliances/robin.md appliances/robin-no-builtins.md"
fi

if [ "${FALDERAL}x" = "x" ]; then
  FALDERAL="falderal -b"
fi

if [ -d dist-newstyle ]; then
  export ROBIN_EXE=`find dist-newstyle -name robin -executable -type f`
elif [ -x "bin/robin.exe" ]; then
  export ROBIN_EXE="bin/robin.exe"
fi

if [ "x$ROBIN_EXE" != "x" ]; then
  echo "Testing executable '$ROBIN_EXE'..."
else
  echo "Testing Robin under runhaskell/runhugs..."
fi

echo "Running tests on core semantics..."
$FALDERAL $APPLIANCES doc/Robin.md || exit 1

if [ "${PACKAGES}x" = "x" ]; then
  PACKAGES="intrinsics small boolean arith list env misc"
fi

for PACKAGE in $PACKAGES; do
    echo "Running tests on '$PACKAGE' package..."
    $FALDERAL $APPLIANCES pkg/$PACKAGE.robin || exit 1
done

if [ "x$FORCE_HUGS" != "x" ] ; then
    echo "Can't run QuickCheck tests with Hugs, skipping"
elif command -v runhaskell 2>&1 >/dev/null ; then
    runhaskell -isrc src/QuickCheckTests.hs || exit 1
fi
