(defun match (pat exp)
  (let ((binding-list (make-array 10 :fill-pointer 0 :adjustable t)))
    (catch 'fail
      (trial (list-match pat exp binding-list) binding-list))))

(defun trial (test bdl)
  (if (not test) 
      (throw 'fail nil)
      (to-list bdl)))

(defun atom-match (pat exp bdl)
  (cond ((not (atom pat)) nil)
        ((is-pattern pat #\=) (equal-pattern-match pat exp bdl))
        ((is-pattern pat #\!) (not-equal-pattern-match pat exp bdl))
        ((is-pattern pat #\>) (greater-pattern-match pat exp bdl))
        ((is-pattern pat #\<) (less-pattern-match pat exp bdl))
        (t (equal pat exp))))

(defun list-match (pat exp bdl)
  (cond ((null pat) (if (null exp) t nil))
        ((atom pat) (atom-match pat exp bdl))
        ((is-pattern-any pat) (any-pattern-match pat exp bdl))
        ((/= (length pat) (length exp)) nil)
        (t (loop for pat-item in pat
              for exp-item in exp
              always (list-match pat-item exp-item bdl)))))

(defun is-pattern-any (pat)
  (equal "&" (string (car pat))))
  
(defun is-pattern (pat pre)
  (and (symbolp pat) (equal pre (char (string pat) 0))))
  
(defun any-pattern-match (pat exp bdl)
  (let ((rules (cdr pat)))
    (if (null rules) t ;;(&) can match any thing
        (loop for rule in rules
             always (atom-match rule exp bdl)))))

(defun equal-pattern-match (pat exp bdl)
  (cond ((is-bind-exist pat bdl)
         (equal (find-binding pat bdl) exp))
        (t (insert pat exp bdl))))

(defun not-equal-pattern-match (pat exp bdl)
  (cond ((not (is-bind-exist (fmt pat) bdl)) nil)
        (t (equal exp (find-binding (fmt pat) bdl)))))

(defun greater-pattern-match (pat exp bdl)
  "pattern looks like '>x'"
  (cond ((not (is-bind-exist (fmt pat) bdl)) nil)
        (t (let ((prev (find-binding (fmt pat) bdl)))
             (if (and (numberp prev) (numberp exp))
                 (> exp prev))))))

(defun less-pattern-match (pat exp bdl)
  (cond ((not (is-bind-exist (fmt pat) bdl)) nil)
        (t (let ((prev (find-binding (fmt pat) bdl)))
             (if (and (numberp prev) (numberp exp))
                 (< exp prev))))))

(defun is-bind-exist (pat bdl)
  (some #'(lambda(x) (equal (car x) pat)) bdl))

(defun find-binding (pat bdl)
 (cdr (find pat bdl :key #'first)))

(defun fmt (pat)
  (intern (concatenate 'string "=" (string-upcase (subseq (string pat) 1)))))

(defun insert (pat exp bdl)
  (vector-push-extend (cons pat exp) bdl) t)

(defun to-list (a)
  (let ((l '())) 
    (every #'(lambda (x) (setq l (cons x l))) a)
    (if (equal 0 (length l)) t l)))


(defun match-test ()
  (let ((test-results 
         '((match '(elephant (color =c) (size =s)) '(elephant (color grey) (size 12)))
           (match '() '())
           (match '(=1) '(1))
           (match '(=a =b !b (& >a <b)) '(1 3 4 2))
           (match '(&) '(a b c))
           (not (match '(=a !a) '(1 1)))
           )))
    (loop for i in test-results
         always i)))











