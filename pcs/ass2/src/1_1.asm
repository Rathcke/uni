bits 32
[SECTION .text]

main:

  jmp short sys_write

  write:
    xor eax, eax          ; clear registers
    xor ebx, ebx
    xor edx, edx

    mov al, 4
    mov bl, 1             ; print to stdout
    pop ecx
    mov dl, 14            ; length of string
    int 0x80              ; execute write
    jmp short sys_execve

  wall:
    xor ebx, ebx          ; clear registers
    xor ecx, ecx
    xor edx, edx

    pop eax               ; pop hello
    push edx              ; push NULL byte
    push 0x6c6c6177
    push 0x2f6e6962
    push 0x2f727375
    push 0x2f2f2f2f       ; push '////usr/bin/wall'
    mov ebx, esp          ; move stackpointer into ebx

    push edx              ; push NULL byte
    mov edx, esp          ; move stackpointer into edx

    push eax              ; push 'hello'
    push ebx              ; push the stackpointer pointing at 'hello'
    mov ecx, esp

    xor edx, edx
    xor eax, eax

    mov al, 11
    int 0x80              ; execute 'wall' with 'hello'

    xor eax, eax
    mov al, 1
    xor ebx, ebx
    int 0x80              ; exit program

  sys_write:
    call write            ; call to print
    db 'Hello, World!', 10

  sys_execve:
    call wall             ; call to wall
    db 'hello'
