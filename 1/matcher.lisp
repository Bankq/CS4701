(defun hello (str)
  (if (string= "h" (car str))
      (car str)
      (cdr str)))

(defun fib (n)
  (do ((iter 1 (+ 1 iter))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= n iter) cur)
    (print cur)))

(defun mypair (Start End)
  (loop
     for x from 1 to Start
     for y from End downto 1
       collect (list x y)))

(defvar *num* 20)
(defun myloop ()
  (loop
       for x from 1 to *num*
       for y from 1 to *num*
       for z from 40 downto *num*
       collect (+ x y z)))



