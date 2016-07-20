  .-------------------------------.
  |  PROACTIVE COMPUTER SECURITY  |
  |    Week 3: Reversing          |
  |      Weekly assignment        |
  '-------------------------------'

1.  Defuse as many phases as you can, you should be able to solve at least the first 3 phases,
    and the last 3 phases is only for bonus points.

2.  Handin a solution.txt which defuses the phases when used as
    ./bomb < solution.txt

2.  Handin a commmented assembly listing of the phases you have defuesed,
    and possibly some puesdo code too. See commented-assembly.txt for an example.

Hints:
======

1. $ objdump -M intel -d bomb
2. $ objdump -x bomb
3. $ readelf -x .data bomb
4. $ readelf -x .rodata bomb
5. $ gdb ./bomb
5a. (gdb) handle SIGALRM ignore
7. $ strace ./bomb


5. To keep your TA happy:
      - Keep you reporting to one or two pages,
      - Don't hand in a Word document, if you need fancy formatting
        and pictures then use PDF; otherwise hand in a text file
        perhaps with some separate graphics files.
