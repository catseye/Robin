#!/bin/sh

find . -name "*.o"  -exec rm {} \;
find . -name "*.hi" -exec rm {} \;
rm -rf pkg/*.robin
