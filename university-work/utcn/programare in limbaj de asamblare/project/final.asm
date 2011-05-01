code	segment para public 'code'
	assume	cs: code, ds: code
	org	100h	; I want to create a .com => this is necessary

entry:
	jmp	main

PSP		dw ?		; my program's PSP value

PROG_ID		db 'bzzzzzzz'	; my program identifier

ID_LGTH		db $ - PROG_ID	; my program identifier's length

INDOS		dd ?		; the indos flag pointer (segment + offset)

CRITERR		dd ?		; the critical error flag pointer (segment + offset)

DISKFLAG	db 0		; if this flag is 0 then a BIOS int 16h is
				; running

OLDINT9		dd ?		; reserve some space for saving the nr. 9
				; interrupt vector I'm going to replace

OLDINT13 	dd ?		; reserve some space for saving the nr. 13
				; interrupt vector I'm going to replace

OLDINT28 	dd ?		; reserve some space for saving the nr. 28
				; interrupt vector I'm going to replace

RUNPRG		db 0		; when this flag is 1, my int 28h
				; will try to run the TSR code

LOCKBIT		db 0		; this will be used as a flag that
				; will tell me whether I'm inside
				; my TSR routine or not. (1 = I'm inside)

LOCKMSK		equ 10001111B	; this is a mask which I use for removing
				; the Num, Caps and Scroll lock bits from
				; the keyboard flag byte

CTRL		equ 00000100B	; this is the bit configuration of the
				; keyboard flag byte when only the Ctrl
				; key is pressed

HOTKEY		equ 44h		; the scan code of F10

mes_ok		db	'File cap.scr has been written.'
mes_vm		db	'Video mode error.             '
mes_er		db	'Error writing file.           '
filename	db	'cap.scr', 0
aux		dw	?
info		db	2000 dup (?)

; procedure that captures the screen and writes it to a file
proc	action far

	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp
	push	sp
	push	es
	push	ds

	mov	ax, 0b800h
	mov	es, ax

; first, we have to create a file to write to

	mov	ah, 3ch		; load "create file" function's code into ah
	mov	cx, 0		; laod new file's attributes into cx (0 = normal file)
	lea	dx, cs:filename	; load filename into dx
	int	21h		; call the DOS function

	mov	aux, ax		; save the returned file handle into memory for later

	jnc	ok1		; check for errors while creating file
	jmp	error

; next, we have to check if video mode is 80x25

ok1:
	mov	ah, 0fh		; load "get current video mode" function's code into ah
	int	10h		; call the BIOS function

	cmp	al, 3		; check if video mode is 80x25
	jz	ok2
	jmp	vmodeerror	; if it isn't, exit with error message

; now we have to copy all information from video memory
; into our memory location skipping the attribute byte
; associated with every character in the video memory

ok2:
	mov	si, 0		; initialize source and destination index
	mov	di, 0
_loop_:
	mov	ah, es:si	; copy from video memory into ah
	mov	cs:info[di], ah ; copy from ah into our memory location
	inc	si		; increment si 2 times to jump over the byte containing
	inc	si		; the attribute of the character
	inc	di
	cmp	si, 4000	; check to see if we read everything or not
	jnz	_loop_		; loop if we still have characters to read

; now we have to write all the information from our memory
; location into the opened file

	mov	ah, 40h		; load "write file" function's code into ah
	mov	bx, aux		; put the saved file handle from memory into bx
	mov	cx, 2000	; load number of bytes to write (80x25) into cx
	lea	dx, info	; load the address containing the information into dx
	int	21h		; call the DOS function

	jc	error		; check for errors
	cmp	ax, cx
	jnz	error

; and, finally, we close the created file

	mov	ah, 3eh		; load "close file" function's code into ah
	int	21h		; call the DOS function

	jc	error		; check for errors

	mov	ax, 0b800h
	mov	es, ax
	mov	si, 0		; initialize source and destination index
	mov	di, 0

loop_m1:
	mov	ah, es:si		   ; copy character from video memory into ah
	mov	cs:info[si], ah		   ; save ah into memory
	mov	ah, byte ptr cs:mes_ok[di] ; copy character from message into ah
	mov	es:si, ah		   ; copy ah into video memory
	inc	si
	mov	ah, es:si		   ; copy attribute from video memory into ah
	mov	cs:info[si], ah 	   ; save ah into memory
	mov	es:si, byte ptr 00000010b  ; set attribute of character (green/black)
	inc	si
	inc	di
	cmp	si, 60			   ; check to see if we saved/wrote everything
	jnz	loop_m1			   ; loop if we still characters to save/read

	jmp	codeend

vmodeerror:
	mov	ax, 0b800h
	mov	es, ax
	mov	si, 0		; initialize source and destination index
	mov	di, 0

loop_m2:
	mov	ah, es:si		   ; copy character from video memory into ah
	mov	cs:info[si], ah		   ; save ah into memory
	mov	ah, byte ptr cs:mes_vm[di] ; copy character from message into ah
	mov	es:si, ah		   ; copy ah into video memory
	inc	si
	mov	ah, es:si		   ; copy attribute from video memory into ah
	mov	cs:info[si], ah 	   ; save ah into memory
	mov	es:si, byte ptr 00000010b  ; set attribute of character (green/black)
	inc	si
	inc	di
	cmp	si, 60			   ; check to see if we saved/wrote everything
	jnz	loop_m2			   ; loop if we still characters to save/read

	jmp	codeend

error:
	mov	ax, 0b800h
	mov	es, ax
	mov	si, 0		; initialize source and destination index
	mov	di, 0

loop_m3:
	mov	ah, es:si		   ; copy character from video memory into ah
	mov	cs:info[si], ah		   ; save ah into memory
	mov	ah, byte ptr cs:mes_er[di] ; copy character from message into ah
	mov	es:si, ah		   ; copy ah into video memory
	inc	si
	mov	ah, es:si		   ; copy attribute from video memory into ah
	mov	cs:info[si], ah 	   ; save ah into memory
	mov	es:si, byte ptr 00000010b  ; set attribute of character (green/black)
	inc	si
	inc	di
	cmp	si, 60			   ; check to see if we saved/wrote everything
	jnz	loop_m3			   ; loop if we still characters to save/read

codeend:

; restore screen

	mov	si, 20000
l1:
	mov	di, 20000
l2:
	dec	di
	jnz	l2
	dec	si
	jnz	l1
	
	mov	si, 0		; initialize source index

restore:
	mov	ah, byte ptr cs:info[si]   ; copy character from old video mem into ah
	mov	es:si, ah		   ; copy ah into video memory
	inc	si
	cmp	si, 60			   ; check to see if we restored everything
	jnz	restore			   ; loop if we still characters to restore

	pop	ds
	pop	es
	pop	sp
	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	ret

action	endp

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
	je	flagcheck
	jmp	_exit_		; if it isn't, don't do a thing

flagcheck:
	mov	ah, 02h		; read the flag byte
	int	16h
	and	al, LOCKMSK	; take Num, Caps and Scroll Lock bits out of the byte
	cmp	al, CTRL	; see if what I have is a CTRL
	je	clearbuffer
	jmp	_exit_		; if they are different, don't do a thing

clearbuffer:
	mov	ah, 01h		; get keyboard status
	int	16h
	jz	bufferok	; if buffer is empty then stop this cycle
	mov	ah, 00h		; if buffer isn't empty then read from it
	int	16h
	jmp	clearbuffer

bufferok:
	cmp	cs:LOCKBIT, 0	; see if I am already running the following code
	je	ok
	jmp	busyexit	; if I am, don't reenter

ok:
	cli
	mov	cs:LOCKBIT, 1	; if no, lock out future reentry tries

	les	bx, cs:INDOS	; es:bx = pointer to INDOS flag
	mov	al, es:[bx]	; al = INDOS flag
	or	al, al		; see if INDOS is 0 => DOS isn't active
	jnz	flagset		; jump if INDOS not zero

	les	bx, cs:CRITERR	; es:bx = pointer to critical error flag
	mov	al, es:[bx]	; al = critical error flag
	or	al, cs:DISKFLAG	; here I check both the critical error flag and
				; the DISKFLAG
	jnz	_exit_		; if al not zero, try again later

	sti
	call	action		; call the procedure that captures
				; the screen and saves it to a file
	mov	RUNPRG, 0	; make sure int 28h will not try to run the program
	jmp	_exit_

flagset:
	mov	RUNPRG, 1	; make sure my int 28h tries to run the program

_exit_:
	mov	cs:LOCKBIT, 0	; ok, I'm done, unlock the procedure

busyexit:
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

; the vector I'm replacing the int 13 vector with
newint13	proc far

	mov	cs:DISKFLAG, 1	; if DISKFLAG = 1 then a disk access is taking place

	pushf
	cli
	call	cs:OLDINT28	; call old interrupt handler
	
	mov	cs:DISKFLAG, 0	; everything's done, so I set DISKFLAG to 0 again

	ret	2		; return from interrupt and preserve flags

newint13	endp

; the vector I'm replacing the int 28 vector with
newint28	proc far

	pushf
	cli
	call	cs:OLDINT28	; call original handler

	cmp	cs:RUNPRG, 1	; do I have to run the program?
	jne	exit28		; exit if not

	cmp	cs:LOCKBIT, 1   ; Is my TSR running?
	je	exit28		; if it is, exit.
	mov	cs:LOCKBIT, 1   ; if it isn't, close gate and go on

	cli

	push	ax		; save everything
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

	les	bx, INDOS		; make sure INDOS flag is no greater than 1
	cmp	byte ptr es:[bx], 1
	ja	exit_restore		; exit if INDOS > 1.

	or	al, DISKFLAG		; see if time-critical disk access is underway
	jnz	exit_restore		; if al not zero, try again later

	sti

	call	action			; call my TSR routine

	mov	cs:RUNPRG, 0		; program has been run

exit_restore:
	pop	es                      ; restore everything
	pop	ds

	assume	ds: nothing

	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	mov	cs:LOCKBIT, 0

exit28:
	iret

newint28	endp

main	proc far

	jmp	start

installmsg:
	db	0dh, 0ah
	db	'The TSR screen capture program is now installed.'
	db	0dh, 0ah
	db	'HOTKEY is CTRL-F10.'
	db	0dh, 0ah, '$'

uninstmsg:
	db	0dh, 0ah
	db	'The TSR screen capture program has been uninstalled.'
	db	0dh, 0ah, '$'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             ;
; check to see if my TSR is already installed ;
; and if it is, uninstall it                  ;
;                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:
	mov	ax, 3509h		; get current interrupt 9 vector
	int	21h
	cmp	bx, offset newint9	; see if its offset is same as my
					; own vector's
	jz	go_on			; if it isn't, it means my TSR is
	jmp	install			; not installed yet

go_on:
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
	jz	uninstall		; if they are not equal => my TSR isn't
	jmp	install			; installed

;;;;;;;;;;;;;;;;;;;;
;                  ;
; uninstall my TSR ;
;                  ;
;;;;;;;;;;;;;;;;;;;;

uninstall:
	lea	dx, cs:uninstmsg	; print uninstall message
	mov	ah, 09h
	int	21h

	cli

	mov	ax, 2509h			; restore old int 9 vector
	mov	dx, word ptr es:OLDINT9		; in ES I have the segment
	mov	ds, word ptr es:OLDINT9[2]	; of my TSR, which was set
	int	21h				; earlier by a int 21h call

	mov	ax, 2513h			; similarly restore old int
	mov	dx, word ptr es:OLDINT13	; 13 vector
	mov	ds, word ptr es:OLDINT13[2]
	int	21h

	mov	ax, 2528h			; and the old int 28 vector
	mov	dx, word ptr es:OLDINT28
	mov	ds, word ptr es:OLDINT28[2]
	int	21h

	mov	bx, es:PSP
	mov	es, bx			; free DOS environment memory
	mov	es, es:[2ch]		; allocated for my TSR
	mov	ah, 49h
	int	21h

	mov	es, bx			; free my own TSR's memory
	mov	ah, 49h
	int	21h

	sti

	mov	ax, 4c00h		; exit the program normally
	int	21h

;;;;;;;;;;;;;;;;;;
;                ;
; install my TSR ;
;                ;
;;;;;;;;;;;;;;;;;;

install:
	mov	ah, 62h			; get the current PSP value and
	int	21h			; store it for using while
	mov	PSP, bx			; uninstalling

	mov	ah, 34h			; get pointer to INDOS flag
	int	21h
	mov	word ptr INDOS, bx	; save aquired pointer
	mov	word ptr INDOS[2], es

	mov	word ptr CRITERR[2], es	; critical error flag has the same segment
	dec	bx			; critical error flag has offset same as INDOS - 1
	mov	word ptr CRITERR, bx

	mov	ax, 3509h		; get old interrupt 9 vector
	int	21h

	mov	word ptr OLDINT9, bx	; save it in OLDINT9
	mov	word ptr OLDINT9[2], es

	mov	ax, 2509h		; install my own interrupt 9 vector
	mov	dx, offset newint9
	int	21h

	mov	ax, 3513h		; get old interrupt 13 vector
	int	21h

	mov	word ptr OLDINT13, bx	; save it in OLDINT13
	mov	word ptr OLDINT13[2], es

	mov	ax, 2513h		; install my own interrupt 13 vector
	mov	dx, offset newint13
	int	21h

	mov	ax, 3528h		; get old interrupt 28 vector
	int	21h

	mov	word ptr OLDINT28, bx	; save it in OLDINT28
	mov	word ptr OLDINT28[2], es

	mov	ax, 2528h		; install my own interrupt 28 vector
	mov	dx, offset newint28
	int	21h

	lea	dx, cs:installmsg	; print install message
	mov	ah, 09h
	int	21h

	mov	dx, main - code		; compute the nr. of paragraphs
	add	dx, 0fh			; the rezident code occupies
	mov	cl, 4
	shr	dx, cl
	mov	ah, 31h			; terminate but stay resident
	int	21h

main	endp

code	ends
	end	entry