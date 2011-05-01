;;; functia inlocuieste elementele din arb care apar in lista
;;; de asociatii lasoc cu elementele asociate lor.
;;; Ex:
;;;	(my-sublis '((1 . UNU) (2 . DOI) (+ . PLUS)) '(+ (+ 1 1) 2) =>
;;;	(PLUS (PLUS UNU UNU) DOI)
(defun my-sublis (lasoc arb)
	(cond
		((assoc arb lasoc) (rest (assoc arb lasoc)))
		((atom arb) arb)
		(T 
			(let 
				(($1 (my-sublis lasoc (first arb)))
				($2 (my-sublis lasoc (rest arb))))
				(cons $1 $2)
			)
			
		)
	)
)

;;; marcheaza fiecare element al listei ls prin setarea
;;; unei proprietati oarecare ($$$) a sale la T.
(defun marcare (ls)
	(mapc #'(lambda (e) (setf (get e '$$$) T)) ls)
)

;;; se colecteaza toate elementele din lista ls care au
;;; proprietatea $$$ setata la T si se inlatura apoi
;;; respectiva proprietate pentru a nu se pune acelasi
;;; element de doua ori
(defun colectare (ls)
	(mapcan #'(lambda (e) (if (remprop e '$$$) (list e) nil)) ls)
)

;;; realizeaza reuniunea a unui numar oarecare de multimi
;;; folosind cele doua functii de mai sus
(defun reun (&rest liste)
	(mapc #'marcare liste)
	(mapcan #'colectare liste)
)

;;; construirea unei "baze de date" de proprietati
(setf (get 'Bogdan 'tata) 'Radu)
(setf (get 'Bogdan 'mama) 'Sanda)
(setf (get 'Dana 'tata) 'Petre)
(setf (get 'Dana 'mama) 'Voichita)
(setf (get 'Andrei 'tata) 'Sandu)
(setf (get 'Andrei 'mama) 'Silvia)
(setf (get 'Danut 'mama) 'Silvia)
(setf (get 'Radu 'tata) 'Petru)
(setf (get 'Radu 'mama) 'Ana)
(setf (get 'Sanda 'tata) 'Victor)
(setf (get 'Sanda 'mama) 'Maria)

;;; functia intoarce bunicul unei persoane
(defun bunic (x)
	(if (get x 'tata) (get (get x 'tata) 'tata))
)

;;; functia intoarce parintii unei persoane
(defun parinti (x)
	(append
		(if (get x 'tata) (list (get x 'tata)))
		(if (get x 'mama) (list (get x 'mama)))
	)
)

;;; functia intoarce toti stramosii unei persoane (incepand cu parintii)
(defun stramosi (x)
	(if (parinti x) (append (parinti x) (mapcan #'stramosi (parinti x))))
)

(defun sqr (x)
	(* x x)
)

(setf pi 3.14159)

;;; arie si perimetru pentru obiectele de tip patrat
(setf (get 'patrat 'arie) #'(lambda (ob) (sqr (get ob 'latura))))
(setf (get 'patrat 'perimetru) #'(lambda (ob) (* 4 (get ob 'latura))))

;;; arie si perimetru pentru obiectele de tip cerc
(setf (get 'cerc 'arie) #'(lambda (ob) (* pi (sqr (get ob 'raza)))))
(setf (get 'cerc 'perimetru) #'(lambda (ob) (* 2 pi (get ob 'raza))))

;;; instantierea proprietatii "tip" a obiectului ob la valoarea parametrului
;;; tipob si a proprietatii cu numele valoarea parametrului prop la valoarea
;;; parametrului valprop pentru acelasi obiect
(defun inst (ob tipob prop valprop)
	(setf (get ob 'tip) tipob)
	(setf (get ob prop) valprop)
)

;;; intoarce perimetru unui obiect oarecare
(defun perimetru (ob)
	(funcall (get (get ob 'tip) 'perimetru) ob)
)

;;; intoarce aria unui obiect oarecare
(defun arie (ob)
	(funcall (get (get ob 'tip) 'arie) ob)
)

;;; definirea unei structuri student
(defstruct student
	(nume "" :type string)
	(prenume "" :type string)
	(nota 5 :type integer)
)

;;; actualizarea proprietatilor unui student
(defun act-student (stud)
	(print "Numele studentului (cu ghilimele in jur)")
	(setf (student-nume stud) (read))
	(print "Prenumele studentului (cu ghilimele in jur)")
	(setf (student-prenume stud) (read))
	(print "Nota studentului (fara ghilimele in jur)")
	(setf (student-nota stud) (read))
	(terpri)
)

;;; afisarea unui student
(defun afis-student (stud)
	(print (student-nume stud))
	(print (student-prenume stud))
	(print (student-nota stud))
	(terpri)
)

;;; initializarea unui vector de studenti
(defun init-vect-stud (vector)
	(do
		((i 0 (+ i 1))
		(n (length vector)))
		((= i n))
		(setf (aref vector i) (make-student))
	)
)

;;; actualizarea unui vector de studenti
(defun act-vect-stud (vector)
	(do
		((i 0 (+ i 1))
		(n (length vector)))
		((= i n))
		(act-student (aref vector i))
	)
)

;;; afisarea unui vector de studenti
(defun afis-vect-stud (vector)
	(do
		((i 0 (+ i 1))
		(n (length vector)))
		((= i n))
		(afis-student (aref vector i))
	)
)

