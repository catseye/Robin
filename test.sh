#!/bin/sh

if [ ! -e bin/robin ]; then
    ./build.sh
fi

falderal test doc/Robin.falderal \
              doc/module/Core.falderal \
              doc/module/Small.falderal \
              doc/module/Exception.falderal \
              doc/module/Concurrency.falderal \
              doc/module/List.falderal
