  .-------------------------------.
  |  PROACTIVE COMPUTER SECURITY  |
  |    Week 2: Shellcode          |
  |      Exercises                |
  '-------------------------------'

= Ex. 1

Write off the following program and assemble it using nasm, then link it using
gcc.

.---- ex1.asm --
| bits 32
| еxtern printf
| global main
| section .text
| main:
|         рush mystr
|         сall printf
|         аdd esp, 4
|         mov eax, 0
|
| ѕection .data
| mystr:
|         db "Hello, world!", 10, 0
'----

Now assemble using nasm:
$ nasm -f elf ex1.asm

This should produce a file called ex1.o.

Now link your program using gcc or ld:
$ gcc -m32 ex1.o -o ex1

You should now have a program called ex1.  Run it:
$ ./ex1

Discuss the following questions:
 1. What do the command line options to nasm and gcc do?
 2. What do nasm and gcc do?
 3. Is the programs entry point the 'main' symbol?  If not then what is?
 4. What is the purpose of the line 'add esp, 4'?
 5. What about 'mov eax, 0'?
 6. What is the ', 10, 0' good for?
 7. Can we use ex1 as shellcode?  Why or why not?

Hints:
1. Examine the program with objdump and readelf:
   $ objdump -d -Mintel ex1
   $ readelf -a ex1

2. As always man(1) is your friend:
   $ man man
   $ man nasm
   $ man gcc
   $ man objdump
   $ man readelf


= Ex. 2

We will repeat exercise 1, but this time we will use write(2) instead of
printf.  Write off this program:

.---- ex2.asm --
| bits 32
| extern write
| global main
| section .text
| main:
|         push 14
|         push mystr
|         push 1
|         call write
|         add esp, 12
|         mov eax, 0
|
| section .data
| mystr:
|         db "Hello, world!", 10
'----

Assemble and link:
$ nasm -f elf ex2.asm
$ gcc -m32 ex2.o -o ex2

Questions:
 1. What are the arguments to write(2)?
 2. What are all the magic numbers in this program?
 3. Why is ', 0' after the string not needed this time?

Hint:
$ man 2 write


= Ex. 3

We will repeat the last exercise but this time we will make the syscall instead
of relying on libc's write(2).  For an explanation of syscalls refer to this
weeks lecture and reading material.

.---- ex3.asm --
| bits 32
| global main
| section .text
| main:
|         mov edx, 14
|         mov ecx, mystr
|         mov ebx, 1
|         mov eax, 4
|         int 0x80
|         mov eax, 0
|
| section .data
| mystr:
|         db "Hello, world!", 10
'----

Hint:
  A lot of nasm constants are included in the include/ folder.  You can use them
  by putting '%include "include/all.asm"' at the top of your program:

.---- ex3b.asm --
| bits 32
| %include "include/all.asm"
| global main
| section .text
| main:
|         mov edx, 14
|         mov ecx, mystr
|         mov ebx, STD_OUT
|         mov eax, SYS_write
|         int 0x80
|         mov eax, 0
|
| section .data
| mystr:
|         db "Hello, world!", 10
'----


= Ex. 4

For this exercise you will write your very first piece of shellcode.  It will be
very simple:  All it should do is simply call 'exit(42)'.

As you should know by now, a shellcode is not a real program, so you can't just
run it.  Instead use the program 'demo'.  It will simply map your shellcode into
memory at a random adress and pass control to it.

Note that you do not need the 'extern', 'global' and 'section' declarations for
shellcode.

When you have your program ready assemble and run it:
$ nasm ex4.asm
$ ./demo ex4

Now check the return value to see that you got 42:
$ echo $?

Questions:
  1. Why is 'extern', 'global' and 'section' declarations not needed?
  2. Why is there no linking step (this is actually the same question as #1)?


= Ex. 5

Now we will step up the game a bit.  Repeat the previous exercise but have the
program print 'goodbye' before exiting.

Where will you put the string?


= Ex. 6

Commandline arguments are passed on the stack.  The number of command line
arguments (usually called 'argc') it stored at '[$ebp + 8]', and a pointer to an
array of pointers to the arguments (usually called 'argv') is stored at '[$ebp +
12]'.

This program will print 'argc':

.---- argc.asm --
| bits 32
| %include "include/all.asm"
|         mov eax, [ebp + 8]
|         add eax, 0x30
|         mov ah, `\n`
|         push eax
|         mov edx, 2
|         mov ecx, esp
|         mov ebx, STD_OUT
|         mov eax, SYS_write
|         int 0x80
|         mov ebx, 0
|         mov eax, SYS_exit
|         int 0x80
'----

The 'demo' program will pass any extra commandline arguments to the program.

Questions:
 1. Why 'add eax, 0x30'?
 2. What happens if there are >8 arguments?


= Ex. 7

Write a shellcode that prints one of the commandline arguments.  You decide
which one.


= Ex. 8

You may have noticed that the 'demo' program mentions how many zero bytes there
are in your shellcode.  Go change all your shellcodes so that number becomes 0.

Why are zero bytes usually bad?
