; example.asm -- quick example file
; assumes uart starts at device address 0x00000000

	; asm's start address
	%begin 0x00000000
	; uart device configuration
	%define UARTCMD 0x00
	%define UARTCINT 0x01
	%define UARTTTY1 0x05
start:
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
	jmp @main

putchar:	; int putchar(char c)
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

putstring:	; int putstring(char *string)
	pusha
	mov r0,sp
	ldc r1,0x44
	sub r0,r1
	ldr r1,r0
	ldc r2,4
.strprint:
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

main:	; void main(void)
	ld r0,@message
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

message:
	%string "Hello World!"
	%byte 13,10,0

