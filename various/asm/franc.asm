;TEST
;programme résidant qui utilise le  vecteur d'interruption 1ch (battements 18.2fois par secondes)
;pour afficher un compteur de 0 à 9 dans le coin suppérieur droit.
;L'int 1ch est détournée.



.model small
.stack
.code
START:

OLD_INT1C dd ?                              ;qui contient l'adresse ou pointe l'ancienne routine d'interruption
SIGNATURE db 'l'
PSP dw ?
VARIABLE db 0
AFFICH db 30h


new1c proc far
        push ax
        push bx
        push dx
        push ds
        push es
        push cx

        inc variable
        cmp variable,18      ;quand variable==18 , +- 1seconde c'est ‚coul‚e
        je inc_affich        ;on incr‚mente affich

        cmp affich,3Ah       
        jne afficher         ;si affiche est diff‚rent de 3ah alors on affiche
        mov affich,30h       ;sinon on le remet … z‚ro
        

afficher:        

        mov ah,3
        mov bh,0
        int 10h

        push dx                ;sauver position curseur

        mov ah,2
        mov bh,0
        mov dh,0
        mov dl,78             ;positionner le curseur ds le coin sup droit
        int 10h

        mov al,affich         ;afficher
        mov cx,1
        mov bh,0
        mov ah,0ah
        int 10h


        pop dx                ;restaurer la postion du curseur
        mov ah,2
        int 10h               ; positioner le curseur a l'endroit sauvegard‚ pr‚cedement

        jmp fin               ; et on saute … fin

inc_affich:

        mov variable,0       ;si variable==18 on la remet … z‚ro
        inc affich

fin:
        pop cx
        pop es
        pop ds
        pop dx
        pop bx
        pop ax

        iret
new1c endp
     
installation:

        mov ax,@code
        mov ds,ax
        
        mov ah,35h
        mov al,1ch
        int 21h
                                            ;sauver l'ancienne int 1ch
       mov word ptr OLD_INT1C,bx            ; ofset
       mov word ptr OLD_INT1C+2,es          ; segment

        push ds

        mov ax,251ch
        mov dx,offset new1c                  ;installer la nouvelle
        mov bx,seg new1c
        mov ds,bx
        int 21h                              ;do it

        pop ds

        mov ah,62h                           ;sauver le segment psp de notre programme
        int 21h
        mov PSP,bx



        mov dx,16+(installation-start+15)/16 ;et sortir en laissant r‚sidant
	mov ax,3100h
        int 21h                              ;ciao


end installation