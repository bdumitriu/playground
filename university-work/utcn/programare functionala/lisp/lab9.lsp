;;; problema pozitionarii a N dame pe tabla de sah
;;; de dimensiuni NxN, in asa fel incat sa nu se
;;; atace reciproc.
(defun dame (n)
	(do_dame nil n)
)

(defun do_dame (conf n)
	(if
		(= (length conf) n)
		;;;(and (print (reverse conf)) (return))
		(print (reverse conf))
		(do*
			(
				(i (if conf (+ (caar conf) 1) 1))
				(j 1 (+ j 1))
			)
			((> j n))
			(if (liber i j conf) (do_dame (cons (cons i j) conf) n))
		)
	)
)

;;; intoarce T daca plasand o dama in pozitia (i,j) ea nu va fi atacata
;;; de nici una din damele deja plasate (pozitiile acestora se afla in
;;; conf).
(defun liber (i j conf)
	(do*
		((lpoz conf (rest lpoz)))
		((null lpoz) T)
		(if
			(or
				(= i (caar lpoz))
				(= j (cdar lpoz))
				(= (+ i j) (+ (caar lpoz) (cdar lpoz)))
				(= (- i j) (- (caar lpoz) (cdar lpoz)))
			)
			(return nil)
		)
	)
)

;;; problema parcurgerii unei table de sah de dimensiune
;;; NxN cu sarituri de cal pornind din coltul din stanga
;;; sus.
(defun sar_cal (n)
	(do_sar_cal '((1 . 1)) n)
)

(defun do_sar_cal (positions n)
	(let 
		((moves '((-1 . 2) (-2 . 1) (-2 . -1) (-1 . -2)
		(1 . -2) (2 . -1) (2 . 1) (1 . 2))))
		(if
			(= (length positions) (* n n))
			(print (reverse positions))
			(do*
				((next_move moves (rest next_move)))
				((null next_move))
				(setf ii (+ (caar positions) (caar next_move)))
				(setf jj (+ (cdar positions) (cdar next_move)))
				(if 
					(ok ii jj positions n)
					(do_sar_cal 
						(cons (cons ii jj) positions)
						n
					)
				)
			)
		)
	)
)

;;; functia intoarce T daca pozitia (i,j) a calului nu este ocupata
;;; si este pe tabla.
(defun ok (i j positions n)
	(and
		(not (member (cons i j) positions :test #'equal))
		(<= i n)
		(>= i 1)
		(<= j n)
		(>= j 1)
	)
)

;;; problema platii unei sume de bani in toate felurile posibile
;;; folosind anumite monede date in lista l_mon.
(defun plata (lis_valori suma)
	(cond
		((null lis_valori) nil)
		((= suma 0) (list (list (list (car lis_valori) 0))))
		((< suma (apply #'min lis_valori)) nil)
		((< suma (car lis_valori)) (plata (rest lis_valori) suma))
		(T (append (plata (rest lis_valori) suma)
			(let ((temp (plata lis_valori (- suma (car lis_valori)))))
				(mapc #'(lambda (sol)
					(adaug-distr (car lis_valori) sol)
				) temp)
				temp
			)
		))
	)
)

(defun adaug-distr (val sol)
	(if
		(assoc val sol)
		(incf (second (assoc val sol)))
		(nconc sol (list (list val 1)))
	)
)

;;; problema acoperirii optimale a unei distante (lung-drum)
;;; prin salturi de cangur avand la dispozitie lista cu
;;; dimensiunile salturilor in list-salturi.
(defmacro cangur (list-salturi lung-drum)
	`(cangur1 (sortare ',list-salturi #'>) ,lung-drum nil)
)

(defun cangur1 (list-salturi drum-ramas sol-partiala &aux tmp)
	(cond
		((= 0 drum-ramas) sol-partiala)
		((null list-salturi) nil)
		((do ((nr-prim-salt (truncate (/ drum-ramas (first list-salturi))) (- nr-prim-salt 1)))
			((= 0 nr-prim-salt))
			(if
				(setf tmp (cangur1
					(cdr list-salturi)
					(- drum-ramas (* nr-prim-salt (first list-salturi)))
					(cons (list (first list-salturi) nr-prim-salt) sol-partiala)
				))
				(return tmp)
			)
		))
		((cangur1 (rest list-salturi) drum-ramas sol-partiala))
	)
)

(defun sortare (lst pred)
	(cond
		((= (length lst) 1) lst)
		(T (cons
			(setq minim
				(do 
					((min (first lst) min)
					(list (rest lst) (rest list)))
					((null list) min)
					(unless (funcall pred min (first list))
					(setq min (first list)))
				)
			)
			(sortare
				(remove-if
					#'(lambda (x) (equal x minim))
				lst) pred
			)
		))
	)
)

(defvar graf1 '((a (b c e)) (b (a d)) (c (b f)) (d (c e)) (e nil) (f (e))))
(defvar graf2 '((a (e c b)) (b (d a)) (c (f b)) (d (e c)) (e nil) (f (e))))

;;; cale intr-un graf
(defun cale1 (start stop graf)
	(cale11 (list start) stop graf)
)

(defun cale11 (cale stop graf)
	(cond
		((eql (first cale) stop) (reverse cale))
		(T (avans1 (cadr (assoc (first cale) graf)) cale stop graf))
	)
)

(defun avans1 (succesori cale stop graf)
	(cond
		((null succesori) nil)
		((member (first succesori) cale) (avans1 (rest succesori) cale stop graf))
		((cale11 (cons (first succesori) cale) stop graf))
		((avans1 (rest succesori) cale stop graf))
	)
)
(defvar graf1 '((a (b c e)) (b (a d)) (c (b f)) (d (c e)) (e nil) (f (e))))

(defun cale2 (start stop graf)
	(cale22 (list (list start)) stop graf)
)

(defun cale22 (drumuri stop graf)
	(cond
		((null drumuri) nil)
		((eql stop (caar drumuri)) (reverse (first drumuri)))
		((cale22
			(extind
				(first drumuri)
				(cadr (assoc (caar drumuri) graf))
				(rest drumuri)
			)
			stop
			graf
		))
	)
)

(defun extind (drum succesori drumuri)
	(cond
		((null succesori) drumuri)
		(
			(member (first succesori) drum) 
			(extind drum (rest succesori) drumuri)
		)
		(T (cons
			(cons (first succesori) drum)
			(extind drum (rest succesori) drumuri)
		))
	)
)