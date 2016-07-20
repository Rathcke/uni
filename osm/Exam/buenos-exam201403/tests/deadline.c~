#include "tests/lib.h"

static const char prog1[] = "[arkimedes]dtest1";
static const char prog2[] = "[arkimedes]dtest2";
static const char prog3[] = "[arkimedes]dtest3";
static const char prog4[] = "[arkimedes]dtest4";
static const char prog5[] = "[arkimedes]dtest5";
static const char prog6[] = "[arkimedes]dtest6";
static const char prog7[] = "[arkimedes]dtest7";

int main(void)
{
  uint32_t child1;
  uint32_t child2;
  uint32_t child3;
  uint32_t child4;
  uint32_t child5;
  uint32_t child6;
  uint32_t child7;

  child1 = syscall_exec(prog1, 15000);
  child2 = syscall_exec(prog2, 0);
  child3 = syscall_exec(prog3, 1000);
  child4 = syscall_exec(prog4, 22500);
  child5 = syscall_exec(prog5, 0);
  child6 = syscall_exec(prog6, 7500);
  child7 = syscall_exec(prog7, 4000);

  syscall_join(child1);
  syscall_join(child2);
  syscall_join(child3);
  syscall_join(child4);
  syscall_join(child5);
  syscall_join(child6);
  syscall_join(child7);
 
  syscall_halt();
  return 0;
}
