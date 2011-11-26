code segment para public 'code'

	assume cs:code, ds:code
	org 100h

start:
	jmp main

	string	db	'abcdef', '$'
	lungime	db	$ - string - 1
	mesaj1	db	13, 10, 'string-ul initial: ', '$'
	mesaj2	db	13, 10, 'string-ul convertit: ', '$'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; macro pentru convertit o litera mica in litera mare corespunzatoare ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
upcs macro char
	mov	al, char	; se pune litera in al
	cmp	al, 'a'		; daca litera < 'a'
	jb	atEnd		; nu se face nimic
	cmp	al, 'z'		; daca litera > 'z'
	ja	atEnd		; nu se face nimic
	sub	al, 32		; litera mica e convertita in mare
atEnd:
endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; macro pentru tiparirea unui string ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
wstr macro string
	mov ah, 9		; nr. functiei Dos pt. afisare string
	mov dx, offset string	; adresa de offset a stringului
	int 21h			; apel de intrerupere Dos
endm

;;;;;;;;;;;;;;;;;;;;;;;
; programul principal ;
;;;;;;;;;;;;;;;;;;;;;;;
main:
	wstr	mesaj1		; se tipareste sirul initial
	wstr	string
	mov	si, 0
	mov	cl, lungime

upper:
	upcs	string[si]
	mov	string[si], al
	inc	si
	loop	upper

	wstr	mesaj2
	wstr	string

exit:
	mov	ax, 4c00h
	int	21h

code ends
end start