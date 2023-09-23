.global main
  main:
    push {fp, lr}
    ldr r0, =#46
    bl putchar
    mov r0, #0
    pop {fp, pc}