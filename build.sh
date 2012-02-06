#!/bin/sh
ghc -package hscurses --make Main.lhs -o bin/robin
rm -f *.o *.hi Robin/*.o Robin/*.hi
