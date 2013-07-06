; Lisp Assignment 5 by Gurpreet Singh 
; Problem 1 
(defun INDEX (n l)
	(if (<= n 0)
		nil
		(if (> n (length l))
				'error
				(let ((x (index (- n 1) (rest l)))) 
					(if (equalp x nil)
						(first l)
						x)
				)
		)
	)
 )

; Problem 2
(defun MIN-FIRST (l)
	(if (endp (rest l))
		l
		(let ((x (min-first (rest l)))) 
			(if (> (first l) (first x))
				(cons (first x) (cons (first l) (rest x)))
				l
			)
		)
	)
)  
				
; Problem 3
(defun SSORT (l)
	(if (endp l)
		nil
		(let* ((l1 (min-first l))
				(x (ssort (rest l1))))
			(cons (first l1) x))
	)
)

; Problem 4
(defun QSORT (l)
	(cond ((endp l) nil)
		(t (let ((pl (partition l (car l)))) 
				(cond ((endp (first pl)) (cons (first l) (qsort (rest l))))
						(t (let ((x (qsort (first pl)))
									(y (qsort (second pl)))) 
								(append  x  y))
						)
				)
			)
		)
	)
)

; Problem 5
(defun MERGE-LIST (l1 l2)
	(cond ((endp l1) l2)
		((endp l2) l1)
		((<= (first l1) (first l2)) (cons (first l1) (merge-list (rest l1) l2)))
		((> (first l1) (first l2)) (cons (first l2) (merge-list l1 (rest l2))))
	)
)

; Problem 6
(defun MSORT (l)
	(if (endp (rest l))
		l
		(let* ((l1 (split-list l)))
				(let ((x (msort (first l1)))
					(y (msort (second l1))))
					(merge-list x y)
				))
	)
)

; Problem 7
(defun REMOVE-ADJ-DUPL (l)
	(if (endp l)
		nil
		(let ((x (remove-adj-dupl (rest l))))
			(if (equalp (first l) (first x))
				x
				(cons (first l) x))
		)
	)
)

; Problem 8
(defun UNREPEATED-ELTS (l)
	(cond ((endp l) nil)
		((or (endp (rest l)) (not (equal (first l) (second l)))) (cons (first l) (unrepeated-elts (rest l)))) 
		((or (endp (cddr l)) (not (equal (first l) (third l)))) (unrepeated-elts (cddr l)))
		(t (unrepeated-elts (rest l))
		)
	)
)

; Problem 9
(defun REPEATED-ELTS (l)
	(cond ((endp l) nil)
		((or (endp (rest l)) (not (equal (first l) (second l)))) (repeated-elts (rest l)))
		((or (endp (cddr l)) (not (equal (first l) (third l)))) (cons (first l) (repeated-elts (cddr l))))
		(t (repeated-elts (rest l)))
	)
)

; Problem 10
(defun COUNT-REPETITIONS (l)
	(if (endp l)
		nil
		(let ((x (count-repetitions (rest l))))
			(if (equal (first l) (second l))
				(append (list (list (+ 1 (caar x)) (cadar x))) (rest x))
				(append (list (list 1 (first l))) x))
		)
	)
)

; Problem 11
(defun SUBSET (f l)
	(if (endp l)
		nil
		(let ((x (subset f (rest l))))
			(if (funcall f (first l))
				(cons (first l) x)
				x)
		)
	)
)

; Problem 12
(defun OUR-SOME (f l) // (our-some #'> '(a b c d))
	(if (endp l)
		nil
		(let ((x (our-some f (rest l))))
				(if (funcall f (first l))
					l
					x))
	)
)
(defun OUR-EVERY (f l)
	(if (endp l)
		t
		(let ((x (our-every f (rest l))))
			(if x (funcall f (first l))
			)
		)
	)
)

; Problem 13
(defun PARTITION1 (f l p)
	(if (endp l)
		(list nil nil)
		(let ((x (partition1 f (rest l) p)))
			(cond ((funcall f (first l) p)
					(append (list (cons (first l) (first x))) (list (second x))))
				(t (append (list (first x)) (list (cons (first l) (second x))))))
		)
	)
)
			

(defun QSORT1 (f l)
	(cond ((endp l) nil)
		(t (let ((pl (partition1 f l (first l))))  
				(cond ((endp (first pl)) (cons (first l) (qsort1 f (rest l))))
						(t (let ((x (qsort1 f (first pl)))
									(y (qsort1 f (second pl)))) 
								(append  x  y))
						)
				)
			)
		)
	)
)

; Problem 14
(defun FOO (f l)
	(if (endp l) () 
		(let* ((x (foo f (rest l))) (ls (rest l))
			(l1 (list (cons (funcall f (first l)) ls)))
			(l2 (mapcar (lambda (a) (cons (first l) a)) x)))
				(append l1 l2))
	)
)


; Problem 15
(defun TR-ADD (l acc)
	(if (endp l)
		acc
		(tr-add (rest l) (+ (first l) acc))
	)
)

(defun TR-MUL (l acc)
	(if (endp l)
		acc
		(tr-mul (rest l) (* (first l) acc))
	)
)

(defun TR-FAC (x acc)
	(if (zerop x)
		acc
		(tr-fac (- x 1) (* x acc))
	)
)

; Problem 16
(defun TRANSPOSE1 (m)
	(cond ((endp (rest m)) (mapcar #'list (first m)))
		(t (mapcar #'cons (first m) (transpose1 (rest m))
		)
	    )
    )
)

(defun TRANSPOSE2 (m)
	(cond ((endp (cdar m)) (list (mapcar #'car m)))
		(t (cons (mapcar #'car m) (transpose2 (mapcar #'rest m))
		   ) 
		)
	)
)
(defun TRANSPOSE3 (m)
	(apply #'mapcar #'list m)
)
			
