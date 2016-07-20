  .-------------------------------.
  |  PROACTIVE COMPUTER SECURITY  |
  |    Week 4: Stack overflows    |
  |      Weekly assignment        |
  '-------------------------------'

For this week's assignment you are given several vulnerable programs:
 - parrot
 - cookiepirate
 - defcon-quals-2004-stage2
 - csaw-2013-exploit-200
 - 31c3-2014-cfy

The first two programs, "parrot" and "cookiepirate", are vulnerable to stack buffer overflows. Locate
the vulnerabilities, and hand in the following:

1. A commented disassembly of the vulnerable functions.

2. A drawing of the vulnerable functions' stack layout, including arguments,
   return address, stack cookie, saved registers, and local variables. Use
   ASCII art, a picture of a hand drawing on a napkin, PostScript, or
   similar.

3. Exploits that will give an attacker arbitrary code execution. You are free
   to choose any (publicly documented) programming language for your exploit,
   but feel free to use doit_parrot.py and doit_cookiepirate.py as templates. To
   demonstrate this, create some shellcode that uses execve to spawn /bin/sh.

4. A description of how you worked around the stack protection.

The other three programs are borrowed from CTF competitions.  Try to solve these
for extra credit only *after* you have completed the steps above.  Remember to
explain your solution(s) in your report.

Note that "31c3-2014-cfy" is an AMD64 binary; writable memory is non-executable.


Hints
    http://pwntools.readthedocs.io/en/2.2/ has documentation for pwntools, which is used in the doit_cookiepirate.py example. Some useful features are illustrated in the example file.

    Stack cookies start with a zero byte, so you might need to overwrite the lower byte to leak the rest.

    Both the assignment tasks have all memory marked as executable. Consider where you can place and find your shellcode.
