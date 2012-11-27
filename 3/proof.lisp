
;; SNF presentation
;; Clause: "Mikes like Emma" : (Likes Mike Emma)
;; Not: (~ CLAUSE)
;; Or: (_or (CLAUSE1 CLAUSE2))
;; And: (CLAUSE1 CLAUSE2)

(setf *random-state* (make-random-state t))

(defun atp (kb target &key (normalize #'snf) (resolution '()))
  " Automated Theorem Prover "
  (resolute (funcall normalize kb) (funcall normalize target) resolution))

(defun resolute (kb target resolution &key (blacklist '()))
  (cond ((null target) nil)
        ((null kb) resolution)
        (t (let ((trail (get-trail kb blacklist)))
             (let ((result (resolute-clause trail target)))
               (cond ((null result)
                      (cons (list trail target result) resolution))
                     ((equal target result) 
                      (resolute kb target resolution :blacklist (cons trail blacklist)))
                      ;; (resolute (cdr kb) target resolution))
                     (t (progn
                          ;; (print result)
                          (cons (list trail target result) (resolute kb result resolution))))))))))

(defun resolute-clause (trail target)
  (block outer
    (cond ((null target) nil)
          ((null trail) target)
          (t (let ((trails (listify trail))
                   (targets (listify target)))
               (loop for a in trails
                  do (loop for b in targets
                        do (let ((theta (try-unify a b)))
                             (if (not (eql 'fail theta))
                                 (return-from outer
                                   (unlistify 
                                    (remove-duplicates 
                                     (subs-var theta 
                                               (eliminate (list a b) 
                                                          (append trails targets)))
                                     :test #'equal)))))))))) 
    target))

(defun get-trail (kb blacklist)
  (let ((l (remove-if #'(lambda (x) (find x blacklist :test #'equal)) kb)))
    (car (sort l #'(lambda (a b) (< (clause-length a) (clause-length b)))))))

(defun clause-length (l)
  (length (listify l)))

(defun listify (c)
  (cond ((or-clause-p c)
         (cdr c))
        (t (list c))))

(defun unlistify (c)
  (cond ((null c) nil)
        ((= 1 (length c))
         (car c))
        (t (append '(or) c))))

(defun try-unify (a b)
  (cond ((opposite-p a b)
         (if (equal (cadr a) b) nil
             (unify (cadr a) b)))
        ((opposite-p b a)
         (if (equal a (cadr b)) nil
             (unify a (cadr b))))
        (t 'fail)))

(defun subs-var (theta clauses)
  (if (null theta)
      clauses
      (subs-var (cdr theta) 
                (subst (cdr (car theta))
                       (car (car theta))
                       clauses
                       :test #'equal))))

(defun eliminate (l c)
  (if (null l)
      c
      (eliminate (cdr l) (single-eliminate (car l) c))))

(defun single-eliminate (c clauses)
  (cond ((null clauses) nil)
        ((equal c (car clauses))
         (single-eliminate c (cdr clauses)))
        (t (cons (car clauses) (single-eliminate c (cdr clauses))))))
            

(defun sentence-p (c)
  (not (or-clause-p c)))

(defun or-clause-p (c)
  (eql 'or (car c)))

(defun not-clause-p (c)
  (if (atom c) nil
      (eql '~ (car c))))

(defun opposite-p (a b)
  (if (and (not-clause-p a) (not (not-clause-p b)))
      t))

(defun unify (x y &optional theta)
  (cond ((eql theta 'fail) 'fail)
        ((eql x y) theta)
        ((varp x) (unify-var x y theta))
        ((varp y) (unify-var y x theta))
        ((and (consp x) (consp y)
              (unify (cdr x) (cdr y) (unify (car x) (car y) theta))))
        (t 'fail)))

(defun unify-var (var x theta)
  (let ((vb (assoc var theta))
        (xb (assoc x theta)))
    (cond (vb (unify (cdr vb) x theta))
          (xb (unify var (cdr xb) theta))
          ((occurs-p var x theta) 'fail)
          (t (cons (cons var x) theta)))))

(defun occurs-p (var x theta)
  (cond ((eql var x) t)
        ((and (varp x) (assoc x theta))
         (occurs-p var (cdr (assoc x theta)) theta))
        ((consp x) (or (occurs-p var (car x) theta)
                       (occurs-p var (cdr x) theta)))
        (t nil)))
                   
(defun varp (x)
  (cond ((null x) nil)
        ((symbolp x) 
         (equal #\= (elt (symbol-name x) 0)))
        (t (and (special-p (car x))
                (varp (cadr x))))))

(defun special-p (x)
  (and (symbolp x)
       (equal #\_ (elt (symbol-name x) 0))))

;; TODO
(defun snf (fol)
  fol)

;; CONST VARIABLES
(defparameter *test-kb-a*  '((or (~ (american =x)) (~ (weapon =y)) (~ (sells =x =y =z)) (~ (hostile =z)) (criminal =x))
                           (american West)
                           (or (~ (missle =x)) (weapon =x))
                           (missle M1)
                           (or (~ (missle =x)) (~ (owns Foo =x)) (sells West =x Foo))
                           (owns Foo M1)
                           (or (~ (enemy =x America)) (hostile =x))
                           (enemy Foo America)))

(defparameter *test-target-a* '(~ (criminal West)))

(defparameter *test-kb-b* '((or (ws =x) (sd =x))
                    (or (~ (ws =y)) (~ (likes =y Waves)))
                    (or (~ (ws =z)) (likes =z Warm))
                    (or (~ (likes Laura =w)) (~ (likes Jacob =w)))
                    (or (likes Jacob =w) (likes Laura =w))
                    (likes Jacob Warm)
                    (likes Jacob Waves)))

(defparameter *test-target-b* '(or (~ (sd =v)) (ws =v)))


(defparameter *test-kb-c* '((or (~ (e =x)) (v =x) (s (_f =x)))
                            (or (~ (e =x)) (v =x) (c (_f =x)))
                            (p d)
                            (e d)
                            (or (~ (s d =y)) (p =y))
                            (or (~ (p =z)) (~ (v =z)))))
(defparameter *test-target-c* '(or (~ (p =w)) (~ (c =w))))
