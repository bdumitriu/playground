;;; factorial fara parametru de acumulare
(defun fact1 (n)
	(cond
		((= n 0) 1)
		(T (* n (fact1 (- n 1))))
	)
)

;;; factorial cu parametru de acumulare
(defun fact2 (n p)
	(cond
		((= n 0) p)
		(T (fact2 (- n 1) (* n p)))
	)
)

;;; copia unei liste
(defun copie (lst)
	(cond
		((atom lst) lst)
		(T (cons (copie (first lst)) (copie (rest lst))))
	)
)

;;; redefinirea functiei equal
(defun my-equal (obj1 obj2)
	(cond
		((and (null obj1) (null obj2)) T)
		((and (numberp obj1) (numberp obj2)) (= obj1 obj2)) 
		((and (stringp obj1) (stringp obj2)) (string= obj1 obj2))
		((and (atom obj1) (atom obj2)) (eq obj1 obj2))
		((or (atom obj1) (atom obj2)) nil)
		((eq obj1 obj2) T)
		((and (listp obj1) (listp obj2))
			(and 
				(our-equal (first obj1) (first obj2))
				(our-equal (rest obj1) (rest obj2))
			)
		)
	)
)

;;; netezire fara parametru de acumulare
(defun netezire1 (list)
	(cond
		((null list) nil)
		((atom (first list)) (cons (first list) (netezire1 (rest list))))
		(T (append (netezire1 (first list)) (netezire1 (rest list))))
	)
)

;;; netezire cu parametru de acumulare
(defun netezire2 (list p)
	(cond
		((null list) p)
		((atom (first list)) (netezire2 (rest list) (append p (list (first list)))))
		(T (netezire2 (rest list) (append p (netezire2 (first list) ()))))
	)
)

;;; problema turnurilor din Hanoi
(defun hanoi (n sursa dest aux)
	(cond
		((= n 1) (print (list 'muta 'de 'pe sursa 'pe dest)))
		(T
			(hanoi (- n 1) sursa aux dest)
			(print (list 'muta 'de 'pe sursa 'pe dest))
			(hanoi (- n 1) aux dest sursa)
		)
	)
	'**ok**
)

;;; reuniunea a 2 multimi reprezentate ca liste
(defun reun (x y)
	(cond
		((null x) y)
		((member (first x) y) (reun (rest x) y))
		(T (cons (first x) (reun (rest x) y)))
	)
)

;;; intersectia a 2 multimi reprezentate ca liste
(defun int (x y)
	(cond
		((null x) nil)
		((member (first x) y) (cons (first x) (int (rest x) y)))
		(T (int (rest x) y))
	)
)

;;; diferenta a 2 multimi reprezentate ca liste
(defun dif (x y)
	(cond
		((null x) nil)
		((member (first x) y) (dif (rest x) y))
		(T (cons (first x) (dif (rest x) y)))
	)
)

;;; testarea incluziunii primei multimi in a doua
(defun inclus (x y)
	(cond
		((null x) T)
		((member (first x) y) (inclus (rest x) y))
		(T nil)
	)
)

;;; testarea disjunctiei a doua multimi
(defun disj (x y)
	(cond
		((null x) T)
		((member (first x) y) nil)
		(T (disj (rest x) y))
	)
)

;;; primeste ca si parametrii ceva de genul
;;; 	(lista 1 '((2) (3) (2 3)))
;;; si intoarce lista
;;;	((1 2) (1 3) (1 2 3))
(defun lista (x y)
	(cond
		((null y) nil)
		(T (cons (cons x (first y)) (lista x (rest y))))
	)
)

;;; multimea partilor unei multimi
(defun parti (x)
	(cond
		((null x) '(nil))
		(T (append
			(lista (first x) (parti (rest x)))
			(parti (rest x))
		))
	)
)

;;; testeaza daca o lista e ordonata crescator
(defun cresc (x)
	(cond
		((null x) T)
		((null (rest x)) T)
		((<= (first x) (second x)) (cresc (rest x)))
		(T nil)
	)
)

;;; eliminarea dintr-o lista a tuturor elementelor nenumerice
(defun elim-nenum (x)
	(cond
		((null x) nil)
		((numberp (first x)) (cons (first x) (elim-nenum (rest x))))
		(T (elim-nenum (rest x)))
	)
)

;;; eliminarea dintr-o lista a tuturor atomilor nenumerici,
;;; indiferent de nivelul de imbricare pe care se afla
(defun elim-atomi-nenum (x)
	(cond 
		((null x) nil)
		((listp (first x)) (cons (elim-atomi-nenum (first x)) (elim-atomi-nenum (rest x))))
		((numberp (first x)) (cons (first x) (elim-atomi-nenum (rest x))))
		(T (elim-atomi-nenum (rest x)))
	)
)

;;; insumarea tuturor atomilor nenumerici de pe nivelul superficial al unei liste
(defun suma-at (x)
	(cond
		((null x) 0)
		((numberp (first x)) (+ (first x) (suma-at (rest x))))
		(T (suma-at (rest x)))
	)
)

;;; insumarea tuturor atomilor nenumerici de pe toate nivelurile unei liste
(defun suma-toti-at (x)
	(cond 
		((null x) 0)
		((listp (first x)) (+ (suma-toti-at (first x)) (suma-toti-at (rest x))))
		((numberp (first x)) (+ (first x) (suma-toti-at (rest x))))
		(T (suma-toti-at (rest x)))
	)
)

;;; procedura de partitionare stanga pt. quicksort
(defun partstg (x cheie)
	(cond
		((null x) ())
		((<= (first x) cheie) (cons (first x) (partstg (rest x) cheie)))
		(T (partstg (rest x) cheie))
	)
)

;;; procedura de partitionare dreapta pt. quicksort
(defun partdr (x cheie)
	(cond
		((null x) ())
		((> (first x) cheie) (cons (first x) (partdr (rest x) cheie)))
		(T (partdr (rest x) cheie))
	)
)

;;; sortarea unei liste
(defun quicksort (x)
	(cond
		((null x) ())
		(T (append 
			(quicksort (partstg (rest x) (first x)))
			(list (first x))
			(quicksort (partdr (rest x) (first x)))
		))
	)
)

;;; calculeaza adancimea maxima a unei liste multinivel
(defun ad-max (x)
	(cond 
		((null x) 1)
		((listp (first x)) (max (+ 1 (ad-max (first x))) (ad-max (rest x))))
		(T (ad-max (rest x)))
	)
)

;;; concatenarea a doua liste
(defun my-2-append (x y)
	(cond
		((null x) y)
		(T (cons (first x) (my-2-append (rest x) y)))
	)
)

;;; concatenarea a n liste
(defun my-append (&rest liste)
	(cond
		((null liste) ())
		((null (rest liste)) (first liste))
		(T (my-2-append (first liste) (my-append (rest liste))))
	)
)