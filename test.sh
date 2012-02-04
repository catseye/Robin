#!/bin/sh

if [ ! -e bin/robin ]; then
    ./build.sh
fi

falderal test doc/Robin.falderal \
              doc/module/Core.falderal \
              doc/module/Small.falderal \
              doc/module/Exception.falderal \
              doc/module/Concurrency.falderal \
              doc/module/List.falderal \
              doc/module/Term.falderal \
              doc/module/Environment.falderal \
              doc/module/Boolean.falderal \
              doc/module/Arithmetic.falderal \
              doc/module/CrudeIO.falderal \
              doc/module/Random.falderal \
              doc/module/Assert.falderal \
              doc/module/Miscellany.falderal
