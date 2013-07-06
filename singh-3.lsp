;Gurpreet Singh   CS 316 Spring 2013
;Ans 1
(defun MIN-2(x y)
   (if(and (numberp x) (numberp y)) 
	 (if(<= x y)
	     x
		 y
	  )
	  'ERROR
	)  
  ) 
		
		
;Ans 2
(defun SAFE-AVG(x y)
     (if(and (numberp x) (numberp y))
        (/ (+ x y) 2)       		
	  nil)     	 
  )
		

;Ans 3
(defun ODD-GT-MILLION(x)
  (if (integerp x)
    (if(oddp x)
	    (if(> x 1000000)
		  T
		  nil)       
	  nil)
   )
)		
		
;Ans 4
(defun MULTIPLE-MEMBER (x y)
	
	(member x (cdr (member x y)))
)

;Ans 5
(defun MONTH->INTEGER(month)
    (if(eq month 'january)
       1
	   (if(eq month 'february)
         2
		 (if(eq month 'march)
           3
		   (if(eq month 'april)
            4
		    (if(eq month 'march)
              5
		      (if(eq month 'june)
               6
		       (if(eq month 'july)
                 7
		         (if(eq month 'august)
                   8
		           (if(eq month 'september)
                     9
		             (if(eq month 'october)
                       10 
		               (if(eq month 'november)
                         11
		                 (if(eq month 'december)
                           12
		                   'ERROR
	 )))))))))))))

;Ans 6
(defun SCORE->GRADE (s)
	(if (numberp s)
		(cond
			((and (>= s 90)) 'A)
			((and (< s 90) (>= s 87)) 'A-)
			((and (< s 87) (>= s 83)) 'B+)
			((and (< s 83) (>= s 80)) 'B)
			((and (< s 80) (>= s 77)) 'B-)
			((and (< s 77) (>= s 73)) 'C+)
			((and (< s 73) (>= s 70)) 'C)
			((and (< s 70) (>= s 60)) 'D)
			((and (< s 60)) 'F)
			(t nil)
			)
		nil)
)
	
;Ans 7
(defun GT (x y)
	(and (numberp x) (numberp y) (> x y))
)

;Ans 8
(defun SAME-SIGN (x y)
	(and (numberp x) (numberp y) (or (and (zerop x) (zerop y)) (and (plusp x) (plusp y)) (and (minusp x) (minusp y))))
)

;Ans 9
(defun SAFE-DIV (x y)
	(and (numberp x) (numberp y) (not (zerop y)) (/ x y))
) 
