#!/bin/sh
ghc --make Main.lhs -o bin/robin
rm -f *.o *.hi Robin/*.o Robin/*.hi
