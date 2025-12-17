%include "common"

%define return_reg r31
%define stack_pointer r30
%define base_pointer r29
%define term_reg r28
%define return_addr_reg r27


%macro push thing
    subs stack_pointer, 1
    st thing, stack_pointer
%endmacro

%macro pop thing
    ld thing, stack_pointer
    adds stack_pointer, 1
%endmacro

%macro call thing
    push return_addr_reg
    jmp return_addr_reg, thing
%endmacro

%macro ret
    mov r2, return_addr_reg
    pop return_addr_reg
    jmp r2
%endmacro

%macro mull x, y
    mul x, x, y
%endmacro

jmp init
global_data_section:
    
init:
    mov term_reg, 0x9F80
                              
    ld r0, term_reg                
    mov r1, { 11 5 << }
    st r1, term_reg, 0x42
    mov r1, { 7 5 << }
    st r1, term_reg, 0x43
    mov r17, 0x1000
    st r17, term_reg, 0x44
    mov r17, 0xF
    st r17, term_reg, 0x46
    mov r17, 1
    st r17, term_reg, 0x45

start:
    mov stack_pointer,2047
	jmp main
main:
	sub stack_pointer, 1
	push base_pointer
	mov base_pointer, stack_pointer
	mov r1, 101
	st r1, base_pointer, 1
.exit_main:
	pop base_pointer
	add stack_pointer, 1
	hlt
__print_unsigned_int:
	test r23, r23
	jnz .__print_unsigned_int_not_zero
	mov r23, '0'
	st r23, term_reg, 0x25
	jmp .__print_unsigned_int_exit
.__print_unsigned_int_not_zero:
	mov r24, 4		; p = 4
.__print_unsigned_int_fixed_point:
	mulh r25, r23, 52429	; q = (n * 52429) >> 16
	shr r25, 3		; q >>= 3
	mul r26, r25, 10		; d*q
	sub r23, r26		; remainder = n - d*q
	st r23, r24, .__print_unsigned_int_buf		
	sub r24, 1		; p--;
	movf r23, r25		; n = q
	jnz .__print_unsigned_int_fixed_point

	add r24, 1
.__print_unsigned_int_print_int:
	ld r23, r24, .__print_unsigned_int_buf
	add r23, '0'
	st r23, term_reg, 0x25
	add r24, 1
	cmp r24, 5
	jne .__print_unsigned_int_print_int
	
.__print_unsigned_int_exit:
	ret
.__print_unsigned_int_buf:
	dw 0, 0, 0, 0, 0

__print_signed_int:
    cmp r23, 0
    jge .__print_signed_int_not_negative
    mov r24, '-'
    st r24, term_reg, 0x25
	xor r23, 65535
    add r23, 1
.__print_signed_int_not_negative:
    call __print_unsigned_int
    ret
    
printf:
.printf_loop:
    ld r24, r23
    test r24, r24
    jz .printf_exit
    st r24, term_reg, 0x25
    add r23, 1
    jmp .printf_loop
.printf_exit:
    ret

putchar:
    st r23, term_reg, 0x25
    ret

getchar:
    ld return_reg, term_reg
    test return_reg, return_reg
    jz getchar
    ret

