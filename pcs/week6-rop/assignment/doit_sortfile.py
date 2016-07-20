#!/usr/bin/env python
import os

DEBUG = False

exploit = "AAAAAAAAAAAAAAAAAAAA"


if DEBUG:
    argv = ["/usr/bin/gdb", "--args"]
else:
    argv = []

argv += ["./sortfile", exploit]

os.execv(argv[0], argv)
