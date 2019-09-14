#!/bin/sh

#./build.sh || exit 1

if [ "${APPLIANCES}x" = "x" ]; then
  APPLIANCES="appliances/robin.md appliances/robin-no-builtins.md"
fi

echo "Running tests on core semantics..."
falderal -b $APPLIANCES doc/Robin.md || exit 1

for PACKAGE in intrinsics small boolean arith list env misc; do
    echo "Running tests on '$PACKAGE' package..."
    falderal -b $APPLIANCES pkg/$PACKAGE.robin || exit 1
done
