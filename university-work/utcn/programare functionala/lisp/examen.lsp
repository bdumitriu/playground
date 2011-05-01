;;; gaseste un drum de la x1,y1 la x2,y2 prin sarituri de cal
;;; unde x1,y1 si x2,y2 pot fi puncte pe o tabla infinita.
;;; varianta mai proasta (da stack overflow pt. distante mari)

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

(defun mutari_dr1 (x x2 y tip_mut)
	(cond
		((<= (- x2 x 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_dr1 (+ x 2) x2 (+ y 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_dr1 (+ x 2) x2 (- y 1) 0)))
	)
)

(defun mutari_dr2 (x x2 y tip_mut)
	(cond
		((<= (- x2 x 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_dr2 (+ x 2) x2 (- y 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_dr2 (+ x 2) x2 (+ y 1) 0)))
	)
)

(defun mutari_st1 (x x2 y tip_mut)
	(cond
		((<= (- x x2 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_st1 (- x 2) x2 (+ y 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_st1 (- x 2) x2 (- y 1) 0)))
	)
)

(defun mutari_st2 (x x2 y tip_mut)
	(cond
		((<= (- x x2 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_st2 (- x 2) x2 (- y 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_st2 (- x 2) x2 (+ y 1) 0)))
	)
)

(defun mutari_sus1 (y y2 x tip_mut)
	(cond
		((<= (- y2 y 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_sus1 (+ y 2) y2 (+ x 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_sus1 (+ y 2) y2 (- x 1) 0)))
	)
)

(defun mutari_sus2 (y y2 x tip_mut)
	(cond
		((<= (- y2 y 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_sus2 (+ y 2) y2 (- x 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_sus2 (+ y 2) y2 (+ x 1) 0)))
	)
)

(defun mutari_jos1 (y y2 x tip_mut)
	(cond
		((<= (- y y2 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_jos1 (- y 2) y2 (+ x 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_jos1 (- y 2) y2 (- x 1) 0)))
	)
)

(defun mutari_jos2 (y y2 x tip_mut)
	(cond
		((<= (- y y2 1) 1) (list (list x y)))
		((= 0 tip_mut) (cons (list x y)
			(mutari_jos2 (- y 2) y2 (- x 1) 1)))
		((= 1 tip_mut) (cons (list x y)
			(mutari_jos2 (- y 2) y2 (+ x 1) 0)))
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
