; Cauta prima aparitie a unui subsir intr-un sir si
; afiseaza pozitia la care este gasit sau -1 daca
; nu este gasit.
data1	segment para public 'data'
	temp	db 5 dup (?)
	final	db '$'

	sir1	db "Hey baby, hey baby, hey!"
	l1	db $ - sir1
data1	ends

data2	segment para public 'data'
	sir2	db "hey!"
	l2	db $ - sir2
data2	ends

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

code	segment para public 'code'
	assume	cs: code, ds: data1, es: data2

main	proc far

	push	ds
	xor	ax, ax
	push	ax
	mov	ax, data1
	mov	ds, ax
	mov	ax, data2
	mov	es, ax

	lea	si, sir1
	cld

loop_:
	push	si
	mov	cl, l2
	xor	ch, ch
	lea	di, sir2
	
	repe	cmpsb
	
	jnz	not_found
	cmp	cx, 0
	jnz	not_found
	jmp	found

not_found:
	pop	si
	inc	si
	mov	ax, si
	lea	bx, sir1
	sub	ax, bx
	cmp	al, l1
	jle	loop_
	jmp	failure

found:
	pop	si
	lea	ax, sir1
	sub	si, ax
	print	si
	jmp	end_

failure:
	mov	si, -1
	mov	ah, 02h
	mov	dl, '-'
	int	21h
	mov	ah, 02h
	mov	dl, '1'
	int	21h

end_:
	ret

main	endp
	
code	ends

	end	main