;;=================================================
;;=================================================

(defun match (pat exp)
  "Top function:
   The matcher should return bindings as follows:
   (match '(elephant (color =c) (size =s)) 
          '(elephant (color grey) (size 12)))"
  (let ((binding-list (make-array 100 :fill-pointer 0 :adjustable t)))
    (catch 'fail
      (trial (list-match pat exp binding-list) binding-list))))

(defun trial (test bdl)
  (if (not test) (throw 'fail nil)
      (to-list bdl)))

(defun atom-match (pat exp bdl)
  "Match two atom var.
   if matched add to the binding list"
  (cond ((not (atom pat)) nil)
        ((is-pattern-equal pat) (equal-pattern-match pat exp bdl))
        ((is-pattern-not-equal pat) (not-equal-pattern-match pat exp bdl))
        ((is-pattern-greater pat) (greater-pattern-match pat exp bdl))
        ((is-pattern-less pat) (less-pattern-match pat exp bdl))
        (t (plain-match pat exp))))

(defun list-match (pat exp bdl)
  (cond ((null pat) (if (null exp) t nil))
        ((atom pat) (atom-match pat exp bdl))
        ((is-pattern-any pat) (any-pattern-match pat exp bdl))
        ((/= (length pat) (length exp)) nil)
        (t (loop for pat-item in pat
              for exp-item in exp
              always (list-match pat-item exp-item bdl)))))

(defun is-pattern-any (pat)
  "Check if a pattern start with a '&' "
  (if (equal "&" (string (car pat)))
      t
      nil))

(defun is-pattern-equal (pat)
  "Check if a pattern start with a ="
  (cond ((symbolp pat)
         (if (equal #\= (char (string pat) 0))
             t
             nil))
        (t nil)))

(defun is-pattern-not-equal (pat)
  "Check if a pattern start with a !"
  (cond ((symbolp pat)
         (if (equal #\! (char (string pat) 0))
             t
             nil))
        (t nil)))

(defun is-pattern-greater (pat)
  "Check if a pattern start with a >"
  (cond ((symbolp pat)
         (if (equal #\> (char (string pat) 0))
             t
             nil))
        (t nil)))

(defun is-pattern-less (pat)
  (cond ((symbolp pat)
         (if (equal #\< (char (string pat) 0))
             t
             nil))
        (t nil)))

(defun is-pattern (pat pre)
  (cond ((symbolp pat)
         (if (equal pre (char (string pat) 0))
             t
             nil))
        (t nil)))

(defun plain-match (pat exp)
  "match symbol name"
  (equal pat exp))
  
(defun any-pattern-match (pat exp bdl)
  (let ((rules (cdr pat)))
    (if (null rules) t ;;(&) can match any thing
        (loop for rule in rules
             always (atom-match rule exp bdl)))))
             

(defun equal-pattern-match (pat exp bdl)
  "pattern looks like '=a'"
  (cond ((is-bind-exist pat bdl)
         (equal (find-binding pat bdl) exp))
        (t (insert pat exp bdl)
           t)))

(defun not-equal-pattern-match (pat exp bdl)
  "pattern looks like '!x'"
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
  "format pattern !name(>name <name) to =name"
  (intern (concatenate 'string "=" (string-upcase (subseq (string pat) 1)))))

(defun insert (pat exp bdl)
  (vector-push (cons pat exp) bdl))

(defun to-list (a)
  (let ((l '())) 
    (every #'(lambda (x) (setq l (cons x l))) a)
    (if (equal 0 (length l)) t l)))














