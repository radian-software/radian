#!/usr/bin/env python3

from __future__ import print_function

import os
import sys

exec_name = sys.argv[0]
args = sys.argv[1:]

if len(args) != 1:
    print("usage: {} <delimiter>".format(exec_name), file=sys.stderr)
    sys.exit(1)

delimiter = args[0]

print(delimiter, end="")
for key, value in os.environ.items():
    if delimiter in key:
        print("delimiter in variable name {}".format(repr(key)),
              file=sys.stderr)
        sys.exit(1)
    if delimiter in value:
        print("delimiter in value for variable {}: {}"
              .format(repr(key), repr(value)), file=sys.stderr)
        sys.exit(1)
    print(key, end="")
    print(delimiter, end="")
    print(value, end="")
    print(delimiter, end="")
