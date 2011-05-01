;;; definim un arbore binar oarecare
(setf a1
	'(14
		(13
			(1
				nil
				nil
			)
			(6
				(5
					nil
					nil
				)
				nil
			)
		)
		(23
			(7
				(9
					nil
					nil
				)
				(10
					nil
					nil
				)
			)
			(8
				nil
				nil
			)
		)
	)
)

;;; definim un arbore ternar folosind liste de asociatii
(setf a2
	'((1 . ((2 . ((2 . nil) (3 . nil) (4 . nil))) (3 . ((5 . nil) (8 . nil) (12 . nil))) (7 . nil))))
)

;;; intoarce lista cu frunzele unui arbore binar oarecare
(defun frunze (arb)
	(cond
		((null arb) nil)
		((and (null (second arb)) (null (third arb))) (list (first arb)))
		(T (append (frunze (second arb)) (frunze (third arb))))
	)
)

;;; intoarce adancimea maxima a unui arbore binar oarecare
(defun adancime (arb)
	(cond
		((null arb) 0)
		((and (null (second arb)) (null (third arb))) 1)
		(T (+ 1 (max (adancime (second arb)) (adancime (third arb)))))
	)
)

(defun inserare (arb elem)
	(cond
		((null arb) (elem . nil))
		((null (first arb)))
	)
)

;;; functii de aici incolo lucreaza pe arbori binari de cautare
;;; de forma: ((cheie . contor) nodst noddr) unde:
;;;	cheie - sir de caractere
;;;	contor - numarul de aparitii
;;;	nodst - lista pt. subarborele stang
;;;	noddr - lista pt. subarborele drept

;;; creare nod
(defun mk-nod (sir &optional (nr 1) st dr)
	(list (cons sir nr) st dr)
)

;;; inserarea unui sir in arbore
;;; daca exista deja, se incrementeaza contorul
(defun inserare (sir arb)
	(cond
		((null arb) (mk-nod sir))
		((string-equal sir (first (first arb)))
			(setf (rest (first arb)) (+ (rest (first arb)) 1))
			arb
		)
		((string-lessp sir (first (first arb)))
			(setf (second arb) (inserare sir (second arb)))
			arb
		)
		(T
			(setf (third arb) (inserare sir (third arb)))
			arb
		)
	)
)

;;; tiparirea unui nod la offs spatii de margine
(defun pr-nod (nod &optional (offs 0))
	(do*
		((i offs (- i 1)))
		((= i 0) (princ (first nod)) (terpri))
		(princ " ")
	)
)

;;; tiparire arbore
(defun tiparire (arb &optional (offs 0))
	(when (not (null arb))
		(tiparire (third arb) (+ offs 4))
		(pr-nod arb offs)
		(tiparire (second arb) (+ offs 4))
	)
)

;;; eliminarea unui nod dintr-un arbore
(defun eliminare (sir arb)
	(cond
		((null arb) nil)
		((string-lessp sir (first (first arb)))
			(setf (second arb) (eliminare sir (second arb)))
			arb
		)
		((string-lessp (first (first arb)) sir)
			(setf (third arb) (eliminare sir (third arb)))
			arb
		)
		((not (= 1 (rest (first arb))))
			(setf
				(rest (first arb)) (- (rest (first arb)) 1)
			)
			arb
		)
		((or (null (second arb)) (null (third arb)))
			(setf arb (or (second arb) (third arb)))
		)
		(T
;;;		...
		)
	)
)