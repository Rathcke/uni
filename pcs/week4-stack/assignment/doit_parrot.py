import os

DEBUG = True

exploit  = "BUILD YOUR "
exploit += "EXPLOIT "
exploit += "HERE!"

if DEBUG:
    argv = ["/usr/bin/gdb", "--args"]
else:
    argv = []

argv += ["./parrot", exploit]

os.execv(argv[0], argv)
