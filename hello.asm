; hello.asm -- hello world!

	%begin 0x00000000
	%define CONSCMD 0x00
	%define CONSOUT 0x01
	%define CONSIN 0x02

_start:
	cint
	ldc op,0
	ldc sp,0xffff
	jmp @main

putchar:
	out.b $CONSOUT,r0
	ret

putstring:
	; pointer to string is in r1
	; uses r0, r1, r2
	ldc r2,1
.loop:
	ldr.b r0,r1	; load byte from *r1
	jz r0,@.done
	call @putchar
	add r1,r2
	jmp @.loop
.done:
	ret

main:
	ldc r1,@message
	call @putstring
.haltloop:
	halt
	jmp @.haltloop

message:
	%string "Hello World!"
	%byte 13,10
	%byte 0
