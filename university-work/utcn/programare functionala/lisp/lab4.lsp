;;; diferenta intre 2 multimi reprezentate ca liste (iterativ)
(defun dif_it (x y)
	(do 
		(result)
		((null x) result)
		(cond 
			((member (first x) y) (setf result result))
			(T (setf result (cons (first x) result)))
		)
		(setf x (rest x))
	)
)

;;; test de incluziune a 2 multimi reprezentate ca liste (iterativ)
(defun inc_it (x y)
	(do
		((result T))
		((null x) result)
		(if (not (member (first x) y)) (return nil))
		(setf x (rest x))
	)
)

;;; lungimea unei liste
(defun lun_it (x)
	(do
		((result 0))
		((null x) result)
		(setf result (+ result 1))
		(setf x (rest x))
	)
)

;;; testeaza daca o lista e ordonata crescator (recursiv)
(defun cresc (x)
	(cond
		((null (rest x)) T)
		((< (first x) (second x)) (cresc (rest x)))
		(T nil)
	)
)

;;; testeaza daca o lista e ordonata crescator (iterativ)
(defun cresc_it (x)
	(do
		((result T))
		((null (rest x)) result)
		(cond
			((< (first x) (second x)) (setf result T))
			(T (return nil))
		)
		(setf x (rest x))
	)
)

;;; eliminarea elementelor nenumerice dintr-o lista
(defun elim_it (x)
	(do
		(result)
		((null x) result)
		(cond
			((numberp (first x))
				(setf result (cons (first x) result))
			)
		)
		(setf x (rest x))
	)
)

;;; eliminarea elementelor nenumerice din lista si din listele 
;;; imbricate in ea s.a.m.d. (recursiv fara parametru de acumulare)
(defun deep_elim (x)
	(cond
		((null x) nil)
		((numberp (first x)) (cons (first x) (deep_elim (rest x))))
		((listp (first x)) 
			(cons (deep_elim (first x)) (deep_elim (rest x)))
		)
		(T (deep_elim (rest x)))
	)
)

;;; eliminarea elementelor nenumerice din lista si din listele 
;;; imbricate in ea s.a.m.d. (recursiv cu parametru de acumulare)
(defun deep_elim_ac (x p)
	(cond
		((null x) p)
		((numberp (first x)) (deep_elim_ac (rest x)
			(append p (list (first x)))
		))
		((listp (first x)) (deep_elim_ac (rest x)
			(append p (list (deep_elim_ac (first x) nil)))
		))
		(T (deep_elim_ac (rest x) p))
	)
)

;;; eliminarea elementelor nenumerice din lista si din listele 
;;; imbricate in ea s.a.m.d. (iterativ)
(defun deep_elim_it (x)
	(do
		(result)
		((null x) result)
		(cond
			((numberp (first x)) (setf result 
				(append result (list (first x)))
			))
			((listp (first x)) (setf result 
				(append result (list (deep_elim_it (first x))))
			))
		)
		(setf x (rest x))
	)
)

;;; insumarea atomilor nenumerici de pe nivelul superficial al unei liste
;;; (iterativ)
(defun suma_at_it (x)
	(do
		((result 0))
		((null x) result)
		(if (numberp (first x)) (setf result (+ result (first x))))
		(setf x (rest x))
	)
)

;;; insumarea atomilor nenumerici de pe toate nivelurile unei liste
;;; (iterativ)
(defun suma_toti_at_it (x)
	(do
		((result 0))
		((null x) result)
		(cond 
			((numberp (first x)) (setf result (+ result (first x))))
			((listp (first x))
				(setf result (+ result (suma_toti_at_it (first x))))
			)
		)
		(setf x (rest x))
	)
)

;;; al n-lea element din sirul lui Fibonacci (recursiv)
(defun fibo (n)
	(cond
		((not (numberp n)) "Dati dracului un numar!")
		((< n 0) "Dati dracului un numar pozitiv!")
		((= n 0) 0)
		((= n 1) 1)
		(T (+ (fibo (- n 1)) (fibo (- n 2))))
	)
)

;;; al n-lea element din sirul lui Fibonacci (iterativ)
(defun fibo_it (n)
	(cond
		((not (numberp n)) "Dati dracului un numar!")
		((< n 0) "Dati dracului un numar pozitiv!")
		((= n 0) 0)
		((= n 1) 1)
		(T (do
			((f1 0 f2)
			 (f2 1 result)
			 (result 0)
			 (contor 1 (+ contor 1)))
			((= contor n) result)
			(setf result (+ f1 f2))
		))
	)
)

;;; lista l trebuie sa fie ceva de genul
;;;	((x ... 1) (y ... 4) (z ... 2) (4 ... 2))
;;; adica o lista de liste de tipul (a ... b)
;;; unde a poate fi orice, iar b trebuie sa fie un numar.
;;; add_element adauga o noua pereche (x / 1) daca x nu e
;;; gasit printre elementele de pe prima pozitie a listelor
;;; ce formeaza lista l sau creste cu 1 valoarea de pe ul-
;;; tima pozitie a listei interioare ce are pe prima pozitie
;;; elementul x.
(defun add_element (x l)
	(cond
		((null l) (list `(,x / 1)))
		((equal x (first (first l))) 
			(setf count (+ (first (last (first l))) 1))
			(append (list `(,x / ,count)) (rest l))
		)
		(T (cons (first l) (add_element x (rest l))))
	)
)

;;; constructia unui multiset (recursiv)
(defun inner_multiset (l p)
	(cond
		((not (listp l)) "Dati dracului o lista!")
		(T
			(cond
				((null l) p)
				(T (inner_multiset (rest l) (add_element (first l) p)))
			)
		)
	)
)

;;; functie user friendly (ca sa nu mai fie nevoit sa dea parametru de 
;;; acumulare)
(defun multiset (l)
	(inner_multiset l ())
)

;;; constructia unui multiset (iterativ)
(defun multiset_it (l)
	(cond
		((not (listp l)) "Dati dracului o lista!")
		(T (do
			(result)
			((null l) result)
			(setf result (add_element (first l) result))
			(setf l (rest l))
		))
	)
)
