#!/bin/sh

if [ ! -e bin/robin -a ! -e bin/robin.exe ]; then
    echo "Please build the robin executable first."
    exit 1
fi

FILES="doc/Fundamental_Semantics.markdown \
      doc/module/Core.falderal \
      doc/module/Small.falderal \
      doc/module/Exception.markdown \
      doc/module/Concurrency.falderal \
      doc/module/List.markdown \
      doc/module/Term.markdown \
      doc/module/Environment.falderal \
      doc/module/Boolean.falderal \
      doc/module/Arithmetic.falderal \
      doc/module/Random.falderal \
      doc/module/Assert.falderal \
      doc/module/Pure.falderal \
      doc/module/CrudeIO.falderal \
      doc/module/Miscellany.falderal"

# Hack for Robin & Falderal built with the ghc from Haskell Platform on Windows
if [ -e bin/robin.exe ]; then
    falderal test -b \
                  -c "Interpret Robin Program" \
                  -c "Interpret Robin Program without output" \
                  -f 'Interpret Robin Program:shell command "bin\\robin.exe -m module -m fixture\\module %(test-file)"' \
                  -f 'Interpret Robin Program without output:shell command "bin\\robin.exe -m module -n %(test-file)"' \
                  ${FILES}
    rm -f results*
else
    falderal test -b \
                  -c "Interpret Robin Program" \
                  -f 'Interpret Robin Program:shell command "bin/robin -m module -m fixture/module %(test-file)"' \
                  ${FILES}
fi
