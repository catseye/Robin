#!/usr/bin/env python

import os
import sys
import re

module = None
major = None
minor = None
filename = None

stuff = {}

if __name__ == '__main__':
    f = open(sys.argv[1], 'r')
    for line in f:
        match = re.match(r'^\-*\s*module\s*(\w+)\s*(\d+)\s*\.?\s*(\d+)\s*$', line)
        if match:
            module = match.group(1)
            major = match.group(2)
            minor = match.group(3)
            filename = "module/%s_%s_%s.robin" % (module, major, minor)
            stuff[filename] = []
            continue
        match = re.match(r'^\-*\s*main\s*$', line)
        if match:
            filename = "unbundled.robin"
            stuff[filename] = []
            continue
        stuff[filename].append(line)
    f.close()

    for filename in stuff.keys():
        f = open(filename, 'w')
        for line in stuff[filename]:
            f.write(line)
        f.close()

    exitcode = (os.system("bin/robin unbundled.robin") / 256)

    for filename in stuff.keys():
        os.unlink(filename)

    sys.exit(exitcode)
