;;=================================================
;;=================================================

(defun match (pat exp)
  "Top function:
   The matcher should return bindings as follows:
   (match '(elephant (color =c) (size =s)) 
          '(elephant (color grey) (size 12)))"
  (let ((binding-list (make-array 100 :fill-pointer 0 :adjustable t)))
    (list-match pat exp binding-list)
    binding-list))

(defun atom-match (pat exp bdl)
  "Match two atom var.
   if matched add to the binding list"
  (if (plain-pat pat)
      (insert pat exp bdl)
      (insert "oh" "no" bdl))

  )

(defun list-match (pat exp bdl)
  (cond ((null pat) (if (null exp)
                        t
                        nil))
        (t (atom-match (car pat) (car exp) bdl)
           (list-match (cdr pat) (cdr exp) bdl)))
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
  
















