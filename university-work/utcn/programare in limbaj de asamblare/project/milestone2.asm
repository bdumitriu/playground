assume	cs: code, ds: code

code	segment para public 'code'

	org	100h	; I want to create a .com => this is necessary

entry:
	jmp	main

OLDINT9	dd ?		; reserve some space for saving the
			; interrupt vector I'm going to replace

LOCKBIT	db 0		; this will be used as a flag that
			; will tell me whether I'm inside
			; my TSR routine or not. (1 = I'm inside)

LOCKMSK	equ 10001111B	; this is a mask which I use for removing
			; the Num, Caps and Scroll lock bits from
			; the keyboard flag byte

CTRL	equ 00000100B	; this is the bit configuration of the
			; keyboard flag byte when only the Ctrl
			; key is pressed

HOTKEY	equ 44h		; the scan code of F10

; the vector I'm replacing the int 9 vector with
newint9	proc far

	pushf
	cli
	call	cs:OLDINT9	; call old interrupt

	push	ax		; save all registers
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp
	push	ds
	push	es

	push	cs
	pop	ds

	assume	ds: code

	in	al, 60h		; read data from keyboard port
	cmp	al, HOTKEY	; see if what I get is equal to my HOTKEY
	jne	_exit_		; if it isn't, don't do a thing
	mov	ah, 02h		; read the flag byte
	int	16h
	and	al, LOCKMSK	; take Num, Caps and Scroll Lock bits out of the byte
	cmp	al, CTRL	; see if what I have is a CTRL
	jne	_exit_		; if they are different, don't do a thing

clearbuffer:
	mov	ah, 01h		; get keyboard status
	int	16h
	jz	bufferok	; if buffer is empty then stop this cycle
	mov	ah, 00h		; if buffer isn't empty then read from it
	int	16h
	jmp	clearbuffer

bufferok:
	cmp	cs:LOCKBIT, 0	; see if I am already running the following code
	jne	_exit_		; if yes, don't reenter
	mov	cs:LOCKBIT, 1	; if no, lock out future reentry tries

	sti

	mov	ah, 0eh		; do some uninteresting thing
	mov	al, 65		; to see that everything works
	int	10h

	mov	cs:LOCKBIT, 0	; ok, I'm done, unlock the procedure

_exit_:
	pop	es		; restore all registers
	pop	ds

	assume	ds: nothing

	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	iret

newint9	endp

            
main	proc near

	mov	ax, 3509h		; get old interrupt 9 vector
	int	21h

	mov	word ptr OLDINT9, bx	; save it in OLDINT9
	mov	word ptr OLDINT9[2], es

	mov	ax, 2509h		; install my own interrupt 9 vector
	mov	dx, offset newint9
	int	21h

	mov	dx, offset installmsg	; print install message
	mov	ah, 09h
	int	21h

	mov	dx, main - code		; compute the nr. of paragraphs
	add	dx, 0fh			; the rezident code occupies
	mov	cl, 4
	shr	dx, cl
	mov	ah, 31h			; terminate but stay resident
	int	21h

installmsg:
	db	0dh, 0ah
	db	'The TSR screen capture program is now installed.'
	db	0dh, 0ah
	db	'HOTKEY is CTRL-F10.'
	db	0dh, 0ah, '$'

main	endp

code	ends
	end	entry