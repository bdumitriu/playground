assume	cs: code, ds: code

code	segment para public 'code'

	org	100h	; I want to create a .com => this is necessary

entry:
	jmp	main

PSP	dw ?		; my program's PSP value
PROG_ID	db 'bzzzzzzz'	; my program identifier
ID_LGTH	db $ - PROG_ID	; my program identifier's length

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             ;
; check to see if my TSR is already installed ;
;                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov	ax, 3509h		; get current interrupt 9 vector
	int	21h
	cmp	bx, offset newint9	; see if its offset is same as my
					; own vector's
	jnz	install			; if it isn't, it means my TSR is
					; not installed yet

	mov	si, offset PROG_ID	; initialize si with my id's length
	mov	di, si
	mov	ax, cs			; as cmpsb compares (ds:si) with
	mov	ds, ax			; (es:di) I need a properly initialized
					; ds (es has been initialized by previous
					; int 21h call)
	mov	cl, ID_LGTH
	mov	ch, 0
	cld				; clear direction flag => si & di will
					; count up
	repe	cmpsb			; repeat comparing string bytes
	jnz	install			; if they are not equal => my TSR isn't
					; installed

	mov	dx, offset uninstmsg	; print uninstall message
	mov	ah, 09h
	int	21h

;;;;;;;;;;;;;;;;;;;;
;                  ;
; uninstall my TSR ;
;                  ;
;;;;;;;;;;;;;;;;;;;;

	cli

	mov	ax, 2509h			; restore old int 9 vector
	mov	dx, word ptr es:OLDINT9		; in ES I have the segment
	mov	ds, word ptr es:OLDINT9[2]	; of my TSR, which was set
	int	21h				; earlier by a int 21h call

	mov	es, PSP			; free DOS environment memory
	mov	es, es:[2ch]		; allocated for my TSR
	mov	ah, 49h
	int	21h

	mov	es, PSP			; free my own TSR's memory
	mov	ah, 49h
	int	21h

	sti

	mov	ax,4c00h		; exit the program normally
	int	21h

;;;;;;;;;;;;;;;;;;
;                ;
; install my TSR ;
;                ;
;;;;;;;;;;;;;;;;;;

install:
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

	mov	ah, 62h			; get the current PSP value and
	int	21h			; store it for using while
	mov	PSP, bx			; uninstalling

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

uninstmsg:
	db	0dh, 0ah
	db	'The TSR screen capture program has been uninstalled.'
	db	0dh, 0ah
	db	'HOTKEY is CTRL-F10.'
	db	0dh, 0ah, '$'

main	endp

code	ends
	end	entry