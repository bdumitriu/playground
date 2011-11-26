code	segment para public 'code'
	assume	cs: code
main	proc far

	push	ds
	xor	ax, ax
	push	ax
	
	in	al, 61h		; read speaker port
	or	al, 3		; set bits 0 & 1
	out	61h, al		; turn speaker on
	mov	cx, 0ffffh	; delay for 4096 units
	
.l1:
	push	cx
	mov	cx, 000ffh

.l2:
	loop	.l2		; time delay

	pop	cx
	loop	.l1
	
	in	al, 61h		; read speaker port
	and	al, 0fch	; clear bits 0 & 1
	out	61h, al		; turn speaker off

	ret
main	endp

code	ends
	end main