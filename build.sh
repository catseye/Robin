#!/bin/sh

MODULES="Core Small Concurrency Exception Random CrudeIO Console"

cat >Robin/Builtins.hs <<EOF
module Robin.Builtins (builtinModules) where

EOF

for MODULE in $MODULES; do
    echo >>Robin/Builtins.hs "import Robin.${MODULE}"
done

cat >>Robin/Builtins.hs <<EOF

builtinModules = [
            (("core",0,1), moduleCore),
            (("small",0,1), moduleSmall),
            (("concurrency",0,1), moduleConcurrency),
            (("exception",0,1), moduleException),
            (("random",0,1), moduleRandom),
            (("crude-io",0,1), moduleCrudeIO),
            (("console",0,1), moduleConsole)
          ]
EOF

ghc -package hscurses --make Main.lhs -o bin/robin
rm -f *.o *.hi Robin/*.o Robin/*.hi
