data	segment para public 'data'
	temp	db 5 dup (?)
	final	db '$'
	
	table	db "0123456789ABCDEF"
	
	; flag masks
	carry	equ 0000000000000001b
	parity	equ 0000000000000100b
	aux	equ 0000000000010000b
	zero	equ 0000000001000000b
	sign	equ 0000000010000000b
	trap	equ 0000000100000000b
	intr	equ 0000001000000000b
	dir	equ 0000010000000000b
	over	equ 0000100000000000b
data	ends

; this macro prints a value contained in the 16 bit register
; received as parameter on the screen
print	macro reg
local	_loop_
	push	ax
	push	bx
	push	dx
	push	di
	
	mov	ax, reg
	mov	bx, 10
	mov	di, 4

_loop_:
	xor	dx, dx
	div	bx
	add	dl, '0'
	mov	temp[di], dl
	dec	di
	cmp	ax, 0
	jnz	_loop_
	
	inc	di
	mov	dx, offset temp
	add	dx, di
	mov	ah, 09h
	int	21h
	
	pop	di
	pop	dx
	pop	bx
	pop	ax
endm

printH	macro reg
local	hex_digit1, hex_digit2, hex_digit3, hex_digit4, _print1, _print2, _print3, _print4
	push	ax
	push	bx
	push	cx
	push	dx
	

	mov	bx, reg

	mov	dl, bh
	mov	cl, 4
	shr	dl, cl
	cmp	dl, 10
	jge	hex_digit1
	add	dl, '0'
	jmp	_print1

hex_digit1:
	add	dl, 'A'
	sub	dl, 10

_print1:
	mov	ah, 02h
	int	21h
	
	mov	dl, bh
	and	dl, 00001111b
	cmp	dl, 10
	jge	hex_digit2
	add	dl, '0'
	jmp	_print2

hex_digit2:
	add	dl, 'A'
	sub	dl, 10

_print2:
	mov	ah, 02h
	int	21h
	
	mov	dl, bl
	mov	cl, 4
	shr	dl, cl
	cmp	dl, 10
	jge	hex_digit3
	add	dl, '0'
	jmp	_print3

hex_digit3:
	add	dl, 'A'
	sub	dl, 10

_print3:
	mov	ah, 02h
	int	21h
	
	mov	dl, bl
	and	dl, 00001111b
	cmp	dl, 10
	jge	hex_digit4
	add	dl, '0'
	jmp	_print4

hex_digit4:
	add	dl, 'A'
	sub	dl, 10

_print4:
	mov	ah, 02h
	int	21h

	pop	dx
	pop	cx
	pop	bx
	pop	ax
endm

; this macro prints an unpacked BCD from the
; 16 bit register received as parameter
print2D	macro reg
	push	ax
	push	bx
	push	dx

	mov	bx, reg

	mov	dl, bh
	add	dl, '0'
	mov	ah, 02h
	int	21h

	mov	dl, bl
	add	dl, '0'
	mov	ah, 02h
	int	21h

	pop	dx
	pop	bx
	pop	ax
endm

; this macro prints an packed BCD from the
; 16 bit register received as parameter
print4D	macro reg
	push	ax
	push	bx
	push	cx
	push	dx
	

	mov	bx, reg

	mov	dl, bh
	mov	cl, 4
	shr	dl, cl
	add	dl, '0'
	mov	ah, 02h
	int	21h
	
	mov	dl, bh
	and	dl, 00001111b
	add	dl, '0'
	mov	ah, 02h
	int	21h
	
	mov	dl, bl
	mov	cl, 4
	shr	dl, cl
	add	dl, '0'
	mov	ah, 02h
	int	21h
	
	mov	dl, bl
	and	dl, 00001111b
	add	dl, '0'
	mov	ah, 02h
	int	21h

	pop	dx
	pop	cx
	pop	bx
	pop	ax
endm

code	segment para public 'code'
	assume	cs: code, ds: data

main	proc far

	push	ds
	xor	ax, ax
	push	ax
	mov	ax, data
	mov	ds, ax

	call	printG
	
	ret

main	endp
	
code	ends

	end	main