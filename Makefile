PROG=robin

all: exe web

exe: pkg/stdlib.robin bin/$(PROG).exe

bin/$(PROG).exe:
ifeq (, $(shell command -v ghc 2>/dev/null))
	echo "ghc not found in PATH, skipping exe build"
else
	(cd src && ghc --make Main.hs -o ../bin/$(PROG).exe)
endif

# For the web build to work, you need parsec installed in a way where haste can use it:
#
#    haste-cabal install parsec-3.1.1
#
# Later versions might not work.  For example, 3.1.13.0 fails to build for me at:
# Preprocessing library generic-deriving-1.12.3...
# src/Generics/Deriving/TH/Pre4_9.hs:177:20:
#     parse error on input ‘->’
#
# The hastec from containerized-hastec comes with parsec already installed this way.
#
# You also need random installed in a way that haste can use it:
#
#    haste-cabal install random-1.1
#

web: pkg/stdlib.robin demo/$(PROG).js

demo/$(PROG).js:
ifeq (, $(shell command -v hastec 2>/dev/null))
	echo "hastec not found in PATH, skipping web build"
else
	(cd src && hastec --make HasteMain.hs -o $(PROG).js && mv $(PROG).js ../demo/$(PROG).js)
endif

pkg/stdlib.robin:
	./build-packages.sh

clean:
	rm -f bin/$(PROG).exe demo/$(PROG).js
	find . -name '*.o' -exec rm {} \;
	find . -name '*.hi' -exec rm {} \;
	find . -name '*.jsmod' -exec rm {} \;
	find pkg -name '*.robin' -exec rm {} \;
