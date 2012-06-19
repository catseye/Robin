#!/bin/sh

if [ ! -e bin/robin -a ! -e bin/robin.exe ]; then
    echo "Please build the robin executable first."
    exit 1
fi

FILES="doc/Fundamental_Semantics.markdown \
      doc/module/Core.markdown \
      doc/module/Small.markdown \
      doc/module/Exception.markdown \
      doc/module/Concurrency.markdown \
      doc/module/Metadata.markdown \
      doc/module/List.markdown \
      doc/module/Term.markdown \
      doc/module/Environment.markdown \
      doc/module/Boolean.markdown \
      doc/module/Arithmetic.markdown \
      doc/module/Random.markdown \
      doc/module/Assert.markdown \
      doc/module/Pure.markdown \
      doc/module/CrudeIO.markdown \
      doc/module/Miscellany.markdown \
      doc/module/Bind-Args.markdown"

FILES_NO_BUILTIN_SMALL="doc/module/Small.markdown"

falderal test -b fixture/config/BuiltInSmall.markdown ${FILES}
falderal test -b fixture/config/SmallInRobin.markdown ${FILES_NO_BUILTIN_SMALL}
