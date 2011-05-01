;;; functia intoarce sublista listei l ce incepe
;;; cu primul element pentru care functia data ca
;;; primul parametru intoarce ceva diferit de NIL.
(defun my-member-if (fun l)
	(cond
		((null l) nil)
		((not (null (funcall fun (first l)))) l)
		(T (member-if fun (rest l)))
	)
)

;;; functia intoarce o structura arborescenta izomorfa
;;; cu cea a listei multinivel date ca si parametru, dar
;;; cu fiecare atom component inlocuit cu rezultatul
;;; aplicarii functie fun pe el.
(defun map-leaf (fun l)
	(cond
		((null l) nil)
		((atom (first l)) 
			(cons (funcall fun (first l)) (map-leaf fun (rest l)))
		)
		(T (cons (map-leaf fun (first l)) (map-leaf fun (rest l))))
	)
)

(defun allfirst (l)
	(cond
		((null l) nil)
		(T (cons (first (first l)) (allfirst (rest l))))
	)
)

(defun allrest (l)
	(cond
		((null l) nil)
		(T (cons (rest (first l)) (allrest (rest l))))
	)
)

(defun my-mapcar (fun &rest l)
	(cond
		((member nil l :test #'equal) nil)
		(T (cons (apply fun (allfirst l)) (apply #'my-mapcar (append (list fun) (allrest l)))))
	)
)

(defun my-remove-if (test lis)
	(mapcan #'(lambda (x) (if (not (funcall test x)) (list x))) lis)
)

(defun f1 (x)
	(if (not (numberp x)) (list x))
)

;;; intoarce o copie a listei argument din care elimina
;;; toate elementele de pe primul nivel care satisfac testul
(defun my-remove-if (test lis)
	(mapcan
		#'(lambda (x)
			(if (funcall test x) nil (list x))
		)
		lis
	)
)

;;; aplica o functie succesiv pe elementele unei liste ca si
;;; MAPCAR pentru o functie de un argument, dar colecteaza doar
;;; rezultatele non-nil
(defun mapcarn (fun lis)
	(mapcan
		#'(lambda (x)
			(if (setf y (funcall fun x)) (list y))
		)
		lis
	)
)

;;; lungimea maxima a sublistelor unei liste
;;; varianta recursiva
(defun lung1 (lis)
	(if
		(atom lis)
		0
		(max (length lis) (lung1 (first lis)) (lung1 (rest lis)))
	)
)

;;; lungimea maxima a sublistelor unei liste
;;; varianta iterativa
(defun lung2 (lis)
	(do
		((rez (length lis))
		(l lis (rest l)))
		((null l) rez)
		(if (listp (first l))
			(setf rez (max rez (lung2 (first l))))
		)
	)
)

;;; lungimea maxima a sublistelor unei liste
;;; varianta cu MAPCAR
(defun lung3 (lis)
	(if
		(atom lis)
		0
		(apply #'max (cons (length lis) (mapcar #'lung3 lis)))
	)
)

;;; numarul de aparitii, pe orice nivel, ale unui atom intr-o lista
(defun aparitii (elem lis)
	(cond
		((null lis) 0)
		((atom lis) (if (equal elem lis) 1 0))
		(T (apply #'+ (mapcar #'(lambda (x) (aparitii elem x)) lis)))
	)
)

;;; numarul de atomi dintr-o lista
(defun nr_atoms (lis)
	(cond
		((null lis) 0)
		((atom lis) 1)
		(T (apply #'+ (mapcar #'nr_atoms lis)))
	)
)

;;; eliminarea parantezelor interioare unei liste
;;; versiunea cu MAPCAR
(defun strivire1 (lis)
	(cond
		((null lis) nil)
		((atom lis) (list lis))
		(T (apply #'append (mapcar #'strivire1 lis)))
	)
)

;;; eliminarea parantezelor interioare unei liste
;;; versiunea cu MAPCAN
(defun strivire2 (lis)
	(cond
		((null lis) nil)
		((atom lis) (list lis))
		(T (mapcan #'strivire2 lis))
	)
)

;;; inversarea elementelor dintr-o lista inclusiv
;;; a celor de pe nivelurile interioare
(defun rev-all (lis)
	(cond
		((atom lis) lis)
		(T (reverse (mapcar #'rev-all lis)))
	)
)

;;; multimea elementelor unei liste
;;; varianta recursiva
(defun mk-set1 (lis)
	(cond
		((null lis) nil)
		((member (first lis) (rest lis)) (mk-set1 (rest lis)))
		(T (cons (first lis) (mk-set1 (rest lis))))
	)
)

;;; multimea elementelor unei liste
;;; varianta cu MAPLIST
(defun mk-set2 (lis)
	(apply #'append 
		(maplist
			#'(lambda (x) 
				(if (member (first x) (rest x)) nil (list (first x)))
			)
			lis
		)
	)
)

;;; multimea elementelor unei liste
;;; varianta cu MAPCON
(defun mk-set3 (lis)
	(mapcon
		#'(lambda (x) 
			(if (member (first x) (rest x)) nil (list (first x)))
		)
		lis
	)
)

;;; reuniunea a doua multimi folosind MAPCON
(defun reun1 (lis1 lis2)
	(mapcon
		#'(lambda (x)
			(if (null (rest x))
				(if (member (first x) lis2)
					lis2
					(cons (first x) lis)
				)
				(if (member (first x) lis2)
					nil
					(list (first x))
				)
			)
		)
		lis1
	)
)

;;; reuniunea a doua multimi folosind MAPC
(defun reun2 (lis1 lis2)
	(let ((rez lis2))
		(mapc
			#'(lambda (x)
				(if (not (member x lis2)) (setf rez (cons x rez)))
			)
			lis1
		)
		rez
	)
)

;;; intersectia a doua multimi folosind functia mapcarn de mai sus
(defun int1 (lis1 lis2)
	(mapcarn
		#'(lambda (x)
			(if (member x lis2) x)
		)
		lis1
	)
)

;;; intersectia a doua multimi folosind MAPCON
(defun int2 (lis1 lis2)
	(mapcon
		#'(lambda (x)
			(if (member (first x) lis2) (list (first x)))
		)
		lis1
	)
)

;;; multimea partilor unei multimi
(defun parti (lis)
	(cond
		((null lis) (list nil))
		((atom lis) (list (list lis)))
		(T
			(let ((temp (parti (rest lis))))
				(append 
					(parti (first lis)) 
					(mapcarn
						#'(lambda (x) 
							(if (null x)
								nil
								(cons (first lis) x)
							)
						)
						temp
					)
					temp
				)
			)
		)
	)
)

;;; multimea partilor unei multimi (mai elegant)
(defun m-parti (set)
	(if (null set)
		'(())
		(extinde-cu (first set) (m-parti (rest set)))
	)
)

(defun extinde-cu (elem set-seturi)
	(append set-seturi
		(mapcar #'(lambda (x) (cons elem x)) set-seturi)
	)
)

;;; intoarce multimea de liste ce rezulta prin
;;; inserarea unui element pe toate pozitiile unei liste
(defun pune-peste-tot (lis elem)
	(setf temp
	(let ((fata nil))
		(maplist
			#'(lambda (x)
				(setf aux (append fata (cons elem x)))
				(setf fata (append (list (first x)) fata))
				aux
			)
			lis
		)
	))
	(setf temp (append temp (list (append lis (list elem)))))
)

;;; multimea permutarilor de elemente ale unei liste
(defun perm (lis)
	(cond
		((null (rest lis)) (list lis))
		((null lis) nil)
		(T
			(mapcan
				#'(lambda (x)
					(pune-peste-tot x (first lis))
				)
				(perm (rest lis))
			)
		)
	)
)

;;; multimea combinarilor de n elemente dintr-o lista
(defun comb (n lis)
	(cond
		((> n (length lis)) nil)
		((= n (length lis)) (list lis))
		((= n 0) '(nil))
		(T
			(append
				(mapcar
					#'(lambda (x)
						(cons (first lis) x)
					)
					(comb (- n 1) (rest lis))
				)
				(comb n (rest lis))
			)
		)
	)
)

;;; multimea aranjamentelor de n elemente dintr-o lista
(defun aranj (n lis)
	(mapcan #'perm (comb n lis))
)