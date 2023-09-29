#!/bin/sh

if [ "${NOBUILD}x" = "x" ]; then
  ./build-packages.sh || exit 1
fi

#
# `bin/robin` is a shell script wrapper which is intended to be convenient
# for command-line usage, but it is not used by this test driver.
#
# Instead, this test driver finds different implementations of robin
# and uses Falderal appliances to test each of those implementations.
#
# It does not test under `runhaskell` by default because it's quite slow
# (slower than `runhugs`, even), and largely redundant if you have built
# an executable with the same `ghc` that `runhaskell` will be using.
# But you can supply your own list of appliances in the `APPLIANCES`
# env var, and include `appliances/runhaskell-robin.md` in it if you like.
#

if [ "${APPLIANCES}x" = "x" ]; then
  if [ -x "bin/robin.exe" ]; then
    APPLIANCES="${APPLIANCES} appliances/robin.exe.md appliances/robin.exe-no-builtins.md"
  fi
  if command -v runhugs 2>&1 >/dev/null ; then
    APPLIANCES="${APPLIANCES} appliances/runhugs-robin.md"
  fi
fi
echo "Implementations under test: ${APPLIANCES}"

if [ "${FALDERAL}x" = "x" ]; then
  FALDERAL="falderal -b"
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

# Note: runhaskell doesn't automatically see any modules
# from the dependencies that are installed when cabal
# builds robin.exe from robin.cabal.  So you may need to
#
#   cabal install --lib random parsec QuickCheck
#
# additionally yourself, to run the QuickCheck tests.

if command -v runhaskell 2>&1 >/dev/null ; then
  runhaskell -isrc src/QuickCheckTests.hs || exit 1
else
  echo "No runhaskell available, not running QuickCheck tests."
fi
