#include "tests/lib.h"

int main(void)
{     
    int a = syscall_getclock();
    printf("Running time is %d ms\n", a);
    int b = syscall_getclock();
    while (b-a < 1000) {
      b = syscall_getclock();
    }
    printf("Running time is now %d ms\n", b);   
    syscall_halt();

    return 0;
}
