bits 32
[SECTION .text]


main:
  xor eax, eax
  mov al, 2         ; fork
  int 0x80          ; execute fork
  cmp eax, 0
  jz child          ; if the cmp is 0, we are in the child process

  parent:
    xor eax, eax    ; clear registers
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx

    push eax        ; push NULL byte
    push 'name'
    push 'in/u'
    push '///b'     ; push '///bin/uname'
    mov ebx, esp    ; move stackpointer into ebx

    push eax        ; push NULL byte
    push '-aaa'     ; push argument to uname '-aaa'
    mov edx, esp    ; move stackpointer into edx
    push eax        ; push NULL byte
    push edx        ; push stackpointer pointing at '-aaa'
    push ebx        ; push stackointer poiting at '///bin/uname'
    mov ecx, esp

    xor edx, edx
    xor eax, eax

    mov al, 11
    int 0x80        ; execute 'uname' with '-aaa'

    xor eax, eax
    mov al, 1
    xor ebx, ebx
    int 0x80        ; exit program

    jmp exit

  child:

    xor eax, eax    ; clear registers
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx

    mov eax, 100
    mov ebx, 0

;  loop:
;    push eax
;    push ebx
;    push eax
;    mov al, 4
;    mov bl, 1       ; write char to stdout
;    mov ecx, esp
;    mov dl, 3
;    int 0x80        ; execute write
;
;    pop ecx         ; pop char in lying on the stack into ecx
;    dec ecx         ; increment ASCII value
;    push ecx        ; push it back onto the stack

;    cmp ecx, 0x2F   ; check if we have written 'Z'
;    je exit         ; jump to exit if we have
;    jmp loop        ; resume the loop if not
;
  exit:
    xor eax, eax
    mov al, 1
    xor ebx, ebx
    int 0x80
