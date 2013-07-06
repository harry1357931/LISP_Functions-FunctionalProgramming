
; Assignment-4 Solutions by Gurpreet Singh
; Section 2(Main Problems)

; Problem 1
(defun SUM (w)
	(if (endp w)
	0
	(let ((y (SUM (rest w))))
		(+ y (first w)))
	)
 )

;Problem 2
(defun NEG-NUMS (w)
	(if (endp w)
	   nil
		(let ((y (NEG-NUMS (rest w))))
			(cond ((minusp (first w)) (cons (first w) y))
			(t y))
	   )
	)
)

;Problem 3
(defun INC-LIST-2 (m n)
	(if (endp m)
		nil
		(let ((x (INC-LIST-2 (rest m) n)))
			(cons (+ n (first m)) x)
		)
	)
 )

;Problem 4
(defun INSERT (k l)
	(if (endp l)
		(list k)
		(if (or (< k (first l)) (= k (first l)))
				(cons k l)
				(let ((x (INSERT k (rest l))))
					(cons (first l) x)
				)
		)
	)
 )

;Problem 5
(defun ISORT (k)
	(if (endp k)
		nil
		(let ((x (ISORT (rest k))))
			(cond ((not x) (list (first k)))
				(t (INSERT (first k) x)))
		)
	)
)

;Problem 6
(defun SPLIT-LIST (k)
	(if (endp k)
		(list nil nil)
		(let ((x (SPLIT-LIST (rest k))))
		(append (list (cons (first k) (second x))) (list (first x))))
	)
)

;Problem 7
(defun PARTITION (n p)
	(if (endp n)
		(list () ())
		(let ((x (PARTITION (rest n) p)))
			(cond ((or (< p (first n)) (= p (first n)))
					(append (list (first x)) (list (cons (first n) (second x)))))
				(t (append (list (cons (first n) (first x))) (list (second x)))))
		)
	)
)

;Problem 8
(defun POS (e f)
	(cond ((endp f) 0)
		((equal e (first f)) 1)
		(t (let ((x (POS e (rest f))))
				(if (not (zerop x))
					(+ x 1)
					0))
	    )
	)
)

;Problem 9
(defun SPLIT-NUMS (m)
	(if (minusp m)
		nil
		(let ((x (SPLIT-NUMS (- m 1))))
			(cond	((evenp m) (append (list (cons m (first x))) (list (second x))))
				(t (append (list (first x)) (list (cons m (second x)))))
		    )
		)
	)
 )

;Problem 10
(defun SET-UNION (m1 m2)
	(if (endp m1)
		m2
		(let ((x (SET-UNION (rest m1) m2)))
			(if (not (member (first m1) x))
				(cons (first m1) x)
				x)
		)
	)
)


;Problem 11
(defun SET-REMOVE (x y)
	(if (not (member x y))
		y
		(let ((z (SET-REMOVE x (rest y)))) 
			(if (not (equalp (first y) x))
				(cons (first y) z)
				z)
		)
	)
 )

;Problem 13
(defun SINGLETONS (d)
	(if (endp d)
		nil
		(let ((x (SINGLETONS (rest d))))
			(if (member (first d) (rest d))
				(set-remove (first d) x)
				(cons (first d) x))
		    )
	)
)
				

;Problem 12
(defun SET-EXCL-UNION (m1 m2)
	(if (endp m1)
		m2
		(let ((x (SET-EXCL-UNION (rest m1) s2)))
			(cond ((member (first m1) x) (set-remove (first m1) x))
				(t (cons (first m1) x)))
		)
	)
)

