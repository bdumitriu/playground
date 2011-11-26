; play the PC speaker with 1-9 keys
stck	segment stack
	db 512 dup (?)
stck	ends

code	segment para public 'code'
	assume	cs: code, ss: stck

; constant declarations
CR	equ 13
LF	equ 10

; variable declarations
pbuf	db ?, ' * 256$'
crlf	db CR, LF, '$'
freqmsg	db 'Frequency (0 to exit) = $'
cntmsg	db ' Hz.$'
	
main	proc far

	push	ds
	xor	ax, ax
	push	ax
	mov	ax, cs		; initialize ds
	mov	ds, ax

	mov	al, 10110110b	; Timer2 from 8253 will function with
				; loading Low Byte, then High Byte with
				; a sqaure wave
	out	43h, al		; write timer control byte to 8253
	
	in	al, 61h		; read speaker port
	or	al, 3		; enable Timer2 & turn on speaker
	out	61h, al		; send control byte to speaker

.mloop:
	mov	dx, offset freqmsg
	mov	ah, 09h
	int	21h		; display message

	mov	ah, 01h		; read a character from the user
	int	21h		; (it should be a number in 0-9)
	
	push	ax
	mov	dx, offset crlf
	mov	ah, 09h
	int	21h		; new line
	pop	ax
	
	cmp	al, '0'		; 0 to quit
	je	.mdone

	push	ax
	mov	dx, offset freqmsg
	mov	ah, 09h
	int	21h		; display message
	pop	ax

	mov	pbuf[0], al

	push	ax
	mov	dx, offset pbuf
	mov	ah, 09h
	int	21h		; display message
	pop	ax

	sub	al, '0'
	mov	ah, 0
	mov	cl, 8
	shl	ax, cl		; user input * 256 = sound frequency (Hz)

	mov	bx, ax
	mov	ax, 34dch	; dx:ax = 1,193,180 (tics/second)
	mov	dx, 12h
	div	bx		; bx = Freq (1/second)

	push	ax
	mov	dx, offset cntmsg
	mov	ah, 09h
	int	21h		; display message

	mov	dx, offset crlf
	mov	ah, 09h
	int	21h		; new line
	pop	ax

	out	42h, al		; Write Low byte to Timer Channel 2
	mov	al, ah
	out	42h, al		; Write High byte to Timer Channel 2

	jmp	.mloop

.mdone:
	in	al, 61h		; read speaker port
	and	al, 0fch	; clear bits 0 & 1
	out	61h, al		; turn speaker off

	ret
main	endp

code	ends
	end main