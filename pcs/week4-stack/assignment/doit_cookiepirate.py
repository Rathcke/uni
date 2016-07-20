#!/usr/bin/env python
from pwn import *
context(arch='i386', os='linux')

# Start the program as a subprocess
# so we can communicate with it
p = process('cookiepirate')

# Read in your shellcode
shellcode = read('shellcode')

# Optional: attach GDB to the process and run some GDB commands
#gdb.attach(p, '''
#    b readline
#''')


# Commuicate with the process
# Other communication commands: recv, recvuntil, send, sendafter
# To see everything sent and received by the process, run with ./doit.py DEBUG
print p.recvline()
p.sendline('John')
print p.recv()
p.sendline('John!')


# Allow the user to read/write to the process (useful when you get a shell!)
p.interactive()
