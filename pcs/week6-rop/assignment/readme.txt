  .-----------------------------------------.
  |  PROACTIVE COMPUTER SECURITY            |
  |    Week 6: Return oriented programming  |
  |      Weekly assignment                  |
  '-----------------------------------------'

The program "sortfile" is vulnerable to a stack buffer overflow.  But it has
ASLR and DEP.  Locate the vulnerability, and hand in the following:

1. A commented disassembly of the vulnerable function(s).

2. A drawing of the vulnerable function(s)' stack layout, including arguments,
   return address, saved registers, and local variables. Use ASCII art, a
   picture of a hand drawing on a napkin, PostScript, or similar.

3. An exploit that will give an attacker arbitrary code execution. You are free
   to choose any (publicly documented) programming language for your exploit,
   but feel free to use doit_sortfile.py as a template. Use your shellcode from the
   previous exercise, or write a new one.

4. A description of how you worked around ASLR and DEP.


When you have completed these steps, you can try your hand at these CTF
challenges for extra credit:
 - pwnium-2014-be-a-robot
 - pctf-2013-pork
 - pctf-2013-dynrpn

Hints:

1. Get control of EIP and the stack just above it.  This is where you place your
   ROP.

2. Hunt for suitable functions and gadgets.  In particular you need to:
     a. Allocate memory at a fixed address.
     b. Move data to the allocated memory.
   Consider the usefulness of imported functions and/or functions in the program
   itself.

3. If you place your shellcode in shellcode.asm, the rules in the Makefile will
   assemble it, convert the output to a C array, and write it to shellcode.h.

   If you compile your exploit to i386 code (-m32 if you are on amd64), you can
   run your exploit in gdb and stay attached when the vulnerable program is
   executed.

   $ gcc -m32 -o exploit exploit.c
   $ gdb ./exploit
