#!/bin/sh

if [ ! -e bin/robin ]; then
    ./build.sh
fi

# Workaround for Falderal hanging; if you run it in
# "messy" mode, it doesn't.  No idea why, yet.

falderal -m test doc/Robin.falderal \
              doc/module/Core.falderal \
              doc/module/Small.falderal \
              doc/module/Exception.falderal \
              doc/module/Concurrency.falderal \
              doc/module/List.falderal \
              doc/module/Environment.falderal \
              doc/module/Boolean.falderal \
              doc/module/Arithmetic.falderal

#             doc/module/CrudeIO.falderal

rm -f GeneratedFalderalTests.sh results*.txt
