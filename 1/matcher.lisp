;;=================================================
;;=================================================

(defun match (pat exp)
  "Top function:
   The matcher should return bindings as follows:
   (match '(elephant (color =c) (size =s)) 
          '(elephant (color grey) (size 12)))"
  (let ((binding-list (make-array 100 :fill-pointer 0 :adjustable t)))
    (list-match pat exp binding-list)
    (to-list binding-list)))

(defun atom-match (pat exp bdl)
  "Match two atom var.
   if matched add to the binding list"
  (insert pat exp bdl)
)

(defun list-match (pat exp bdl)
  (cond ((null pat) (if (null exp)
                        t
                        nil))
        (t (loop for pat-item in pat
             for exp-item in exp
                if (atom pat-item)
                do (atom-match pat-item exp-item bdl)
                else do (list-match pat-item exp-item bdl))))
)


(defun plain-pat (pat)
  "Test if pattern if a plain var"
  (if (null (find (char (write-to-string pat) 0) 
                  (vector "=" "!" "&" ">" "<")))      
      t
      nil))

(defun insert (pat exp bdl)
  (vector-push (cons pat exp) bdl))


(defun to-list (a)
  (let ((l '()))
    (every #'(lambda (x) (setq l (cons x l)))
           a)
    l))
  
















