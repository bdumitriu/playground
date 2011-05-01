;;; gaseste un drum de la x1,y1 la x2,y2 prin sarituri de cal
;;; unde x1,y1 si x2,y2 pot fi puncte pe o tabla infinita.
;;; varianta mai buna pt. ca nu da stack overflow (e facuta nerecursiv)

(defun drum_cal (x1 y1 x2 y2)
	(cond
		((or (< x1 1) (< x2 1) (< y1 1) (< y2 1)
		     (> x1 64000) (> x2 64000) (> y1 64000) (> y2 64000)) nil)
		(T (drum_cal_ x1 y1 x2 y2))
	)
)

(defun drum_cal_ (x1 y1 x2 y2)
	(cond
		((and (<= x1 x2) (<= y1 y2)) (drum_cal_dr_sus x1 y1 x2 y2))
		((and (>= x1 x2) (<= y1 y2)) (drum_cal_st_sus x1 y1 x2 y2))
		((and (<= x1 x2) (>= y1 y2)) (drum_cal_dr_jos x1 y1 x2 y2))
		((and (>= x1 x2) (>= y1 y2)) (drum_cal_st_jos x1 y1 x2 y2))
	)
)

(defun mutari_dr1 (x1 x2 y1 tip_mut)
	(do
		((drum nil) (x x1 (+ x 2)) (y y1))
		((<= (- x2 x 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if
			(= 0 tip_mut)
			(and (setf y (+ y 1)) (setf tip_mut 1))
			(and (setf y (- y 1)) (setf tip_mut 0))
		)
	)
)

(defun mutari_dr2 (x1 x2 y1 tip_mut)
	(do
		((drum nil) (x x1 (+ x 2)) (y y1))
		((<= (- x2 x 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if
			(= 0 tip_mut)
			(and (setf y (- y 1)) (setf tip_mut 1))
			(and (setf y (+ y 1)) (setf tip_mut 0))
		)
	)
)

(defun mutari_st1 (x1 x2 y1 tip_mut)
	(do
		((drum nil) (x x1 (- x 2)) (y y1))
		((<= (- x x2 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if
			(= 0 tip_mut)
			(and (setf y (+ y 1)) (setf tip_mut 1))
			(and (setf y (- y 1)) (setf tip_mut 0))
		)
	)
)

(defun mutari_st2 (x1 x2 y1 tip_mut)
	(do
		((drum nil) (x x1 (- x 2)) (y y1))
		((<= (- x x2 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if
			(= 0 tip_mut)
			(and (setf y (- y 1)) (setf tip_mut 1))
			(and (setf y (+ y 1)) (setf tip_mut 0))
		)
	)
)

(defun mutari_sus1 (y1 y2 x1 tip_mut)
	(do
		((drum) (y y1 (+ y 2)) (x x1))
		((<= (- y2 y 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if 
			(= 0 tip_mut)
			(and (setf x (+ x 1)) (setf tip_mut 1))
			(and (setf x (- x 1)) (setf tip_mut 0))
		)
	)
)

(defun mutari_sus2 (y1 y2 x1 tip_mut)
	(do
		((drum) (y y1 (+ y 2)) (x x1))
		((<= (- y2 y 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if 
			(= 0 tip_mut)
			(and (setf x (- x 1)) (setf tip_mut 1))
			(and (setf x (+ x 1)) (setf tip_mut 0))
		)
	)
)

(defun mutari_jos1 (y1 y2 x1 tip_mut)
	(do
		((drum) (y y1 (- y 2)) (x x1))
		((<= (- y y2 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if 
			(= 0 tip_mut)
			(and (setf x (+ x 1)) (setf tip_mut 1))
			(and (setf x (- x 1)) (setf tip_mut 0))
		)
	)
)

(defun mutari_jos2 (y1 y2 x1 tip_mut)
	(do
		((drum) (y y1 (- y 2)) (x x1))
		((<= (- y y2 1) 1) (reverse (cons (list x y) drum)))
		(setf drum (cons (list x y) drum))
		(if 
			(= 0 tip_mut)
			(and (setf x (- x 1)) (setf tip_mut 1))
			(and (setf x (+ x 1)) (setf tip_mut 0))
		)
	)
)

(defun ok_poz (i j drum x y)
	(and
		(not (null i))
		(not (null j))
		(not (member (list i j) drum :test #'equal))
		(not (> i (+ x 2)))
		(not (> j (+ y 2)))
		(not (< i (- x 2)))
		(not (< j (- y 2)))
	)
)

(defun do_cal (drum x2 y2 mut)
	(cond
		((equal (first (last drum)) (list x2 y2)) drum)
		(T (do_cal (append
			drum
			(do
				((move mut (rest move)) (i) (j))
				((ok_poz i j drum x2 y2) (list (list i j)))
				(setf i (+ (caar move) (caar (last drum))))
				(setf j (+ (cdar move) (second (first (last drum)))))
			)
		) x2 y2 mut))
	)
)

(defun mutari_sf (x1 y1 x2 y2)
	(let ((mut '((-1 . 2) (1 . 2) (2 . -1) (1 . -2) (-1 . -2) (-2 . -1) (-2 . 1) (2 . 1))))
		(do_cal (list (list x1 y1)) x2 y2 mut)
	)
)

(defun drum_cal_dr_sus (x1 y1 x2 y2)
	(let* (
		(mut_dr (mutari_dr1 x1 x2 y1 0))
		(mut_sus (mutari_sus1
			(second (first (last mut_dr)))
			y2
			(first (first (last mut_dr)))
			0
		))
		(mut_sf (mutari_sf (first (first (last mut_sus))) 
			(first (rest (first (last mut_sus)))) x2 y2))
	)
		(append mut_dr (rest mut_sus) (rest mut_sf))
	)
)

(defun drum_cal_dr_jos (x1 y1 x2 y2)
	(let* (
		(mut_dr (mutari_dr2 x1 x2 y1 0))
		(mut_jos (mutari_jos1
			(second (first (last mut_dr)))
			y2
			(first (first (last mut_dr)))
			0
		))
		(mut_sf (mutari_sf (first (first (last mut_jos))) 
			(first (rest (first (last mut_jos)))) x2 y2))
	)
		(append mut_dr (rest mut_jos) (rest mut_sf))
	)
)

(defun drum_cal_st_sus (x1 y1 x2 y2)
	(let* (
		(mut_st (mutari_st1 x1 x2 y1 0))
		(mut_sus (mutari_sus2
			(second (first (last mut_st)))
			y2
			(first (first (last mut_st)))
			0
		))
		(mut_sf (mutari_sf (first (first (last mut_sus))) 
			(first (rest (first (last mut_sus)))) x2 y2))
	)
		(append mut_st (rest mut_sus) (rest mut_sf))
	)
)

(defun drum_cal_st_jos (x1 y1 x2 y2)
	(let* (
		(mut_st (mutari_st2 x1 x2 y1 0))
		(mut_jos (mutari_jos2
			(second (first (last mut_st)))
			y2
			(first (first (last mut_st)))
			0
		))
		(mut_sf (mutari_sf (first (first (last mut_jos))) 
			(first (rest (first (last mut_jos)))) x2 y2))
	)
		(append mut_st (rest mut_jos) (rest mut_sf))
	)
)
