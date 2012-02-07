#!/bin/sh

MODULES="Small Concurrency Exception Random CrudeIO Console"

cat >Robin/Builtins.hs <<EOF
module Robin.Builtins (builtinModules) where

import qualified Robin.Core
EOF

for MODULE in $MODULES; do
    echo >>Robin/Builtins.hs "import qualified Robin.${MODULE}"
done

cat >>Robin/Builtins.hs <<EOF

builtinModules = [
EOF

for MODULE in $MODULES; do
    echo >>Robin/Builtins.hs "    (Robin.${MODULE}.moduleId, Robin.${MODULE}.moduleDef),"
done

cat >>Robin/Builtins.hs <<EOF
    (Robin.Core.moduleId, Robin.Core.moduleDef)
  ]
EOF

ghc -package hscurses --make Main.lhs -o bin/robin
rm -f *.o *.hi Robin/*.o Robin/*.hi
