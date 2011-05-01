; Ctrl+F12 = 8a / 138

data segment para public 'data'
	message		db	'Screen captured.$'
	errormes	db	'An error occured.$'
	wrongmode	db	'Program only works for 80x25 video mode.$'
	filename	db	'capture.scr', 0
	aux		dw	?
	info		db	2000 dup (?)
data ends

code segment para public 'code'

	assume cs:code, ds:data, es: nothing, ss:nothing

start:
	push	ds		; standard init stuff begins
	xor	ax, ax
	push	ax
	mov	ax, data
	mov	ds, ax		; standard init stuff ends
	mov	ax, 0b800h
	mov	es, ax		; make es the base video memory address

loop1:
	mov	ah, 08h
	int	21h		; read a character from the user
	cmp	al, 0		; look for an extended ascii code which
	jne	loop1		; is supposed to start with a zero
	int	21h		; read the extended ascii code
	cmp	al, 138		; look for a Ctrl+F12
	jne	loop1		; loop if no Ctrl+F12 found

; first, we have to create a file to write to

	mov	ah, 3ch		; load "create file" function's code into ah
	mov	cx, 0		; laod new file's attributes into cx (0 = normal file)
	lea	dx, filename	; load filename into dx
	int	21h		; call the DOS function

	mov	aux, ax		; save the returned file handle into memory for later

	jc	error		; check for errors while creating file

; next, we have to check if video mode is 80x25

	mov	ah, 0fh		; load "get current video mode" function's code into ah
	int	10h		; call the BIOS function

	cmp	al, 3		; check if video mode is 80x25
	jnz	vmodeerror	; if it isn't, exit with error message

; now we have to copy all information from video memory
; into our memory location skipping the attribute byte
; associated with every character in the video memory

	mov	si, 0		; initialize source and destination index
	mov	di, 0
loop2:
	mov	ah, es:si	; copy from video memory into ah
	mov	info[di], ah 	; copy from ah into our memory location
	inc	si		; increment si 2 times to jump over the byte containing
	inc	si		; the attribute of the character
	inc	di
	cmp	si, 4000	; check to see if we read everything or not
	jnz	loop2		; loop if we still have characters to read

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

	mov	ah, 09h		; print a success message
	lea	dx, message
	int	21h

	jmp	codeend

vmodeerror:
	mov	ah, 09h		; print an error message
	lea	dx, wrongmode
	int	21h
	jmp	codeend

error:
	mov	ah, 09h		; print an error message
	lea	dx, errormes
	int	21h

codeend:
	mov	ax, 4c00h	; standard exit stuff
	int	21h

code ends

end start