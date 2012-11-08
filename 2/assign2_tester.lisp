(require 'puzzle "./puzzle.lisp")
(defparameter hw2testset
	(list
		'((0 1 2 3 4 5 6 7 8) 0)
		'((1 0 2 3 4 5 6 7 8) 1)
		'((1 2 0 3 4 5 6 7 8) 2)
        '((1 2 5 3 4 0 6 7 8) 3)
	)
)

(defparameter hw2-goal '(0 1 2 3 4 5 6 7 8)) 
(defun hw2-goalp (x) (equal x hw2-goal))

(defun hw2-nextstate (state i action)
	;(format t "~a ~a ~a ~%" state i action)
	(let ((newstate (copy-seq state)))
		(cond 
			((equalp action "up")
				(if (> i 2)
					(progn
						(rotatef (elt newstate i) (elt newstate (- i 3))) 
						newstate)
					nil))
			((equalp action "down")
				(if (< i 6)
					(progn
						(rotatef (elt newstate i) (elt newstate (+ i 3))) 
						newstate)
					nil))
			((equalp action "left")
				(if (> (mod i 3) 0)
					(progn 
						(rotatef (elt newstate i) (elt newstate (- i 1))) 					
						newstate)					
					nil))
			((equalp action "right")
				(if (< (mod i 3) 2)
					(progn 
						(rotatef (elt newstate i) (elt newstate (+ i 1)))
						newstate)
					nil))
			(t nil)
		))
)

(defun hw2-zero-loc (state)
	(dotimes (k (length state) nil)
		(when (equal (elt state k) 0)
			(return k)))
)

(defun hw2-check-sequence (state actions)
	;(format t "~a ~a ~a ~%" state (hw2-zero-loc state) (car actions))
	(cond
		((null state) nil)
		((hw2-goalp state) (null actions))
		((null actions) nil)
		(t (hw2-check-sequence (hw2-nextstate  state (hw2-zero-loc state) (car actions)) (cdr actions)))
	)		
)


(defun hw2test ()
	(let ((total 0) (correct 0))
		(dolist (x hw2testset)
			(progn
				(incf total)
				(let ((nocrash nil))
					(ignore-errors
						(let 
							(
								(res1 (8-puzzle (car x) #'misplaced))
								(res2 (8-puzzle (car x) #'manhattan))
							)
							;(format t "~a ~a ~%" res x)
							(if (and (eq (cadr x) (cadr res1)) (hw2-check-sequence (car x) (car res1))
									(eq (cadr x) (cadr res2)) (hw2-check-sequence (car x) (car res2)))
								(progn
									(incf correct)								
									(format t "Correct in this example ~a ~a ~%" (car x) (cadr x))
								)
								(format t "Error in this example ~a Corret Length: ~a Your Length: ~a ~a ~%" (car x) (cadr x) (cadr res1) (cadr res2))
							)
						)
						(setf nocrash t)						
					)
					(unless nocrash (format t "Crash in this case: ~a ~%" (car x)))
				)
			)
		)
		(format t "total: ~a correct: ~a score: ~a ~%" total correct (/ correct total))
	)
)

;(defun misplaced nil nil)
;(defun manhattan nil nil)
;(defun extracredit nil nil)

;(defun 8-puzzle (state heu)
;	'(("left") 1 0))

;(8-puzzle '(0 1 2 3 4 5 6 7 8) #'manhattan)
