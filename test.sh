#!/bin/sh

if [ ! -e bin/robin -a ! -e bin/robin.exe ]; then
    echo "Please build the robin executable first."
    exit 1
fi

FILES="doc/Robin.falderal \
      doc/module/Core.falderal \
      doc/module/Small.falderal \
      doc/module/Exception.falderal \
      doc/module/Concurrency.falderal \
      doc/module/List.falderal \
      doc/module/Term.falderal \
      doc/module/Environment.falderal \
      doc/module/Boolean.falderal \
      doc/module/Arithmetic.falderal \
      doc/module/Random.falderal \
      doc/module/Assert.falderal \
      doc/module/Pure.falderal"

# Hack for Robin & Falderal built with the ghc from Haskell Platform on Windows
if [ -e bin/robin.exe ]; then
    falderal test -b \
                  -c "Interpret Robin Program" \
                  -f 'Interpret Robin Program:shell command "bin\\robin.exe %(test-file)"' \
                  ${FILES}
else
    FILES="${FILES} \
           doc/module/CrudeIO.falderal \
           doc/module/Miscellany.falderal"
    falderal test -b ${FILES}
fi

