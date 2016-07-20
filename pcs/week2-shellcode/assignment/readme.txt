  .-------------------------------.
  |  PROACTIVE COMPUTER SECURITY  |
  |    Week 2: Shellcode          |
  |      Weekly assignment        |
  '-------------------------------'

Programming
===========

For this assignment you can choose between the tasks listed below. They are
split into categories by difficulty.  To be eligible for the maximum of 3 points
you must solve at least three tasks from the 'easy' category OR two tasks from
the 'medium' category OR one task from the 'hard' category.  Other combinations
will be graded at your TA's judgement.

In each task you must write assembly code for an i386 Linux system (e.g. the
VM).  The code should be position independent and contain no NUL bytes.

You should try to make the code as small as possible.

On entry, the instruction pointer (EIP) points to the first byte of your code.
The stack pointer (ESP) is valid (i.e. it points to a sufficiently large
readable-writable section of memory).

You can use AT&T or Intel syntax at your own preference. (But we
prefer Intel if you need input to your decision.)

Hand in the (well commented) assembly code.

----------------------
-- Category 1: easy --
----------------------

1: Warm-up
----------

Your shellcode should first write the text "Hello, World!" and a line feed
character to STDOUT (file descriptor 1), then executes /usr/bin/wall with the
single command line argument "hello".

2: Looping
----------

Your shellcode should first call fork(2).  Using a loop, the child should use
write(2) to print each letter of the alphabet to STDOUT.  The parent should
execute /bin/uname with the single argument "-a".

3: Counting
---------

The same as task 2 but the child should instead print the numbers from 100 to 0
(in decimal).

4: Fibonacci
------

The same as task 2 but the child should print the first 20 fibonacci numbers.

Task 2-4 bonus
--------------

Wait for the child to finish before executing /bin/uname.

------------------------
-- Category 2: medium --
------------------------

5: Cat
------

Your shellcode should open(2) the file /etc/passwd, then in a loop read its
contents and write them to STDOUT.

6: Plumbing
-----------

Your shellcode should create a pipe(2), then fork(2).  The child should write
the string "Hello, world" to the pipe.  The parent should read from the pipe and
then write what was read to STDOUT.

7: Listing
----------

Your shellcode should use getdents(2) to list the contents of the directory /etc
and write them to STDOUT.

Bonus: Sort the directory entries before printing them.
Bonus: Also print sizes, permissions, etc.

----------------------
-- Category 3: hard --
----------------------

8: Anti-IDS
-----------

In this task you must write a shellcode to circumvent the latest advances in IDS
technology; your shellcode should execute /bin/sh, but encrypt its input/output.

Here is how:
 1) Create two pipes -- one for the shell's STDIN and one for its STDOUT.
 2) Fork
 In the parent:
   a) Call dup2(2) to copy the read-end of pipe 1 to STDIN, and the write-end
      of pipe 2 STDOUT.
   b) Execute /bin/sh
 In the child:
   a) Fork
   In the child:
     1) Read from STDIN
     2) Exit if EOF is reached
     3) Decrypt the data that was read
     4) Write the decrypted data to the write-end of pipe 1
     5) Go to 1
   In the parent:
     1) Read from the read-end of pipe 2
     2) Exit if EOF is reached
     3) Encrypt the data that was read
     4) Write the encrypted data to STDOUT
     5) Go to 1

For encryption/decryption we recommend the ROT13 algorithm, or Quadruple-ROT13
for a larger security margin.

9: Linked
---------

In this task you must write a (very) short shellcode that searches through a
randomly generated linked list until it finds an entry with the tag 0x41414100.
Your code has to return a pointer to the data entry of the node with the correct
tag.

Your shellcode must be shorter than or exactly 17 bytes.

The list structure looks like this:

  typedef struct _llist {
    struct _llist *next;
    uint32_t tag;
    char data[100];
  } llist;

This is the code that runs your shellcode:

  func = (char *(*)(llist *))userBuf;
  out = func(head);

  write(STDOUT_FILENO, out, strlen(out));
  write(STDOUT_FILENO, "\n", 1);

  exit(0);

When your shellcode is executed, the return address is at the top of the stack
followed by a pointer to the head of the linked list:

  ESP + 4 : llist* head
  ESP     : return address

The program "linked" will randomly generate a linked list, then read up to 17
bytes of shellcode on STDIN and execute it.  The data field of the node with tag
0x41414100 will be populated with the first command line argument.  This is the
expected behavior:

  $ nasm linked-shellcode.asm
  $ ./linked "after all, we're all alike" < linked-shellcode
  after all, we're all alike

10: Show and tell
-----------------

You decide!  Be sure to cover why, what and how in your report.

Refer to the other tasks for the intended level of difficulty.


Reporting
=========

State which task(s) you have solved.  Shortly outline the structure of your code
to make it comprehensible for your TA.

Explain which challenges you encountered and what you did to overcome them.

Describe how you made your shellcode NUL byte free, and what you did to make it
small.


Hints
=====

 1) Read "Practical Linux Shellcode" (reading/linux_shellcodes.pdf).
 2) Solve the lab exercises (especially ex. 4).
 3) In a terminal type each of the following and press enter:
      man 1 man
      man 2 fork
      man 2 pipe
      man 2 execve
      man 2 dup2
      man 2 wait
      man 2 getdents
 4) To keep your TA happy:
      - Keep you reporting to one or two pages,
      - Don't hand in a Word document, if you need fancy formatting
        and pictures then use PDF; otherwise hand in a text file.
      - If you hand in more than one file with source code, then
        include a Makefile that builds everything as the default
        target.
