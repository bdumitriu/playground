;;; echivalent al functiei nconc
(defun my_nconc (l1 l2)
	(setf (rest (last l1)) l2)
	l1
)

;;; inversarea unei liste folosind in mod distructiv
;;; nu merge...
(defun inv_belita (l)
	(cond
		((null l) nil)
		((null (rest l)) l)
		(T
			(let 
				((temp1 (list (first l))))
				(setf l (rest l))
				(setf (rest (last (inv l))) temp1)
			)
		)
	)
)

(defun inv (lista)
	(do
		((ante) (curent) (urm lista))
		((endp urm) ante)
		(setf curent urm)
		(setf urm (rest urm))
		(setf (cdr curent) ante)
		(setf ante curent)
	)
)

;;; macrodefinitie pentru actualizarea unei proprietati cu
;;; o valoare
(defmacro my-putprop (ob val prop)
	`(setf (get (quote ,ob) (quote ,prop)) (quote ,val))
)

;;; macrodefinitie pentru o forma de tip IF false THEN t1 ELSE t2
(defmacro if-not (cond t1 &optional t2)
	`(if ,cond ,t2 ,t1)
)

;;; reimplementare gresita or, varianta 1
(defun my-or1 (&rest args)
	(do*
		((loc-args args (rest loc-args)) (rez))
		((null loc-args))
		(if (setf rez (eval (first loc-args))) (return rez))
	)
)

;;; reimplementare gresita or, varianta 2
(defun my-or2 (args)
	(do*
		((loc-args args (rest loc-args)) (rez))
		((null loc-args))
		(if (setf rez (eval (first loc-args))) (return rez))
	)
)

;;; reimplementare gresita or, varianta 3
(defun my-or3 (&rest args)
	(do*
		((loc-args args (rest loc-args)) (rez))
		((null loc-args))
		(if (setf rez (eval (first loc-args))) (return `(quote ,rez)))
	)
)

;;; reimplementare corecta or, cu macrodefinitie
(defmacro my-or (&rest args)
	`(or ,(car args) (my-or ,@(rest args)))
)

;;; exemplu de destructurare
(defmacro ar-if (test neg zero (par impar))
	`(let
		((,var ,test))
		(cond
			((< ,var 0) ,neg)
			((= ,var 0) ,zero)
			(T
				(cond
					((= 0 (mod ,var 2)) ,par)
					(T ,impar)
				)
			)
		)
	)
)

;;; functie automodificabila
;;; Trece, in functie de variabila globala varsta, prin
;;; doua perioade: acumularea cunostintelor si aplicarea lor.
(defun evolutie (x)
	(cond
		((< varsta 7) (acumuleaza x) (setf varsta (+ 1 varsta)))
		(T (setf (symbol-function 'evolutie)
			(list
				(car (symbol-function 'evolutie))
				(cadr (symbol-function 'evolutie))
				(car (last (symbol-function 'evolutie)))
			)
		))
	)	
	(aplica x)
)

(defun acumuleaza (x)
	(print `(acumuleaza cu parametrul ,x))
)

(defun aplica (x)
	(print `(aplica cu parametrul ,x))
	(terpri)
)

(defun test ()
	(do
		((i 0 (+ i 1)))
		((= i 10))
		(print `(varsta ,varsta))
		(evolutie i)
	)
)