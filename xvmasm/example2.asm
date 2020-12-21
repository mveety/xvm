; example.asm -- quick example file
; assumes uart starts at device address 0x00000000

	; program start address
	%begin 0x00000000
	; uart device configuration
	%define UARTCMD 0x00
	%define UARTCINT 0x01
	%define UARTTTY1 0x05
	; load the required plugins
	; %load "preproc-repeat.lisp"
start2:
	; save pc into r1 to set up relative addressing
	; note: the pc is at the start of the instruction during
	; the execution of the instruction. it's not incremented
	; until the successful execution of the instruction.
	; also note: if running under an OS OP should be set
	; to the entry point of the program already and if not in
	; system mode to begin with the ops on FL and OP will be
	; basically no-ops
	mov r1, pc
	; go into system mode, set up absolute addressing
	mov r0, fl
	bclrc r0,0
	bclrc r0,1
	mov fl,r0
	; set offset pointer to the start of the program
	mov op,r1
	bsetc r0,1
	mov fl,r0
	; be sure to set up the stack
	ldc r0,@stack
	add r0,op	; assume relative addressing
	mov sp,r0
	jmp @main

putchar2:	; int putchar(char c)
	; this writes to the UART
	pusha
	mov r0,sp
	ldc r1,0x44	; 16 registers pushed, 4 bytes each + 4 bytes for the arg
	sub r0,r1
	ldr r1,r0
	ldc r2,0
	out.w $UARTCMD,r2	; write data in TTY1
	out.b $UARTTTY1,r1	; data to write
	dint $UARTCINT		; do the write
	popa
	ldc r0,0	; return 0
	ret

	%include "example.asm"

putstring2:	; int putstring(char *string)
	pusha
	mov r0,sp
	ldc r1,0x44
	sub r0,r1
	ldr r1,r0
	ldc r2,1
.strprint:
	ldc r3,0
	ldr.b r3,r1
	jz r3,@.exitloop
	push r3
	call @putchar
	add r1,r2
	jmp @.strprint
.exitloop:
	popa
	ldc r0,0	; return 0
	ret

main2:	; void main(void)
	ld r0,@message2
	push r0
	call @putstring
	; disabled interrupts the old fashioned way
	mov r0,fl
	bclrc r0,2
	mov fl,r0
	; you could also do this:
	; cint
.haltloop:
	halt
	jmp @.haltloop

stack:
	%repeat 1024 %byte 0
.end:	; stack end plus some buffer
	%word 0 0 0 0

message2:
	%string "Hello World from example 2!"
	%byte 13,10,0

