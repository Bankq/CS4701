;;===============================================================
;;            Automated Theorem Prover
;;    W4701 Artificial Intelligence Homework 3, Fall 2012
;;    Author: Hang Qian
;;    UNI: hq2124
;;    Contact: hq2124(at)columbia.edu
;;
;; Copyright (c) 2012, Hang Qian
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies, 
;; either expressed or implied, of the FreeBSD Project.
;;===============================================================

;;===============================================================
;;                Input and Output format
;; SNF presentation
;; Clause: "Mikes like Emma" : (Likes Mike Emma)
;; Vars: *prefix with #\=* : "Anyone Mike Likes" : (Likes Mike =X)
;; Not: (~ CLAUSE) : "Mike doesn't like Emma" : (~ (Likes Mike Emma))
;; Or: (or (CLAUSE1 CLAUSE2 ...)) "Mike likes Emma or Mike is a guy" : (or (Likes Mike Emma) (Guy Mike))
;; And: (CLAUSE1 CLAUSE2 ...) : "Mike likes Emma and Mike is a guy" : ((Likes Mike Emma) (Guy Mike))
;; Skolem functions: *prefix with #\_*: (Likes =x (f =x)) => (Likes =x (_f =x))
;;
;; Input:
;; 1) kb: An *AND* list of clauses. Each clause is a sentence/or-clause/not-clause
;; 2) target: A sentence/or-clause/not-clause
;; NOTICE: target should not be a *AND* list
;; 3) normalize: function convert any FOL to SNF. NOTICE:NOT IMPLEMENTED
;;
;; Output:
;; The resolution list (r1 r2 r3 ...), each is a triplet (Clause1 Clause2 resolute-result)
;;===============================================================

;;===============================================================
;;               Test Case
;; There are 3 test case at the bottom which are all 3 examples in class lecture.
;; If anyone need to test it, just run
;; (atp *test-kb-a* *test-target-a*)
;; (atp *test-kb-b* *test-target-b*)
;; (atp *test-kb-c* *test-target-c*)
;;===============================================================

(defun atp (kb target &key (normalize #'snf) (resolution '()))
  (resolute (funcall normalize kb) (funcall normalize target) resolution))

(defun resolute (kb target resolution &key (blacklist '()))
  (cond ((null target) nil)
        ((null kb) resolution) ;If there's no knowledge, we are done.
        (t (let ((trail (get-trail kb blacklist))); get a trail from kb to try to resolute with target
             (let ((result (resolute-clause trail target)))
               (cond ((null result); getting a null means we are done
                      (cons (list trail target result) resolution))
                     ((equal target result); getting the old same target means fail
                      (if (= (length kb) (length blacklist)); if we have tried everything in kb, we are done
                          resolution
                          (resolute kb target resolution :blacklist (cons trail blacklist)))); or put the failed one in black list and do it again
                     (t (cons (list trail target result) (resolute kb result resolution)))))))))

(defun resolute-clause (trail target)
  "resolute two clauses"
  (block outer
    (cond ((null target) nil)
          ((null trail) target)
          (t (let ((trails (listify trail)) ;listify takes a or-clause and turn it into list of sentences/not-clause
                   (targets (listify target)))
               (loop for a in trails
                  do (loop for b in targets ; loop over trails and targets
                        do (let ((theta (try-resolute a b)))
                             (if (not (eql 'fail theta));if a b can be resoluted ( maybe with unification)
                                 (return-from outer; we are done
                                   (unlistify ;turn it back to or-clause if possible
                                    (remove-duplicates ; remove the duplicates
                                     (subs-var theta ; substitute vars using unification result
                                               (eliminate (list a b) (append trails targets))); remove a b since they are resoluted
                                     :test #'equal)))))))))) 
    target)); if all fails, return the old same target

(defun get-trail (kb blacklist)
  "Choose from knowledge base to resolute"
  (let ((l (remove-if #'(lambda (x) (find x blacklist :test #'equal)) kb))) ; First we remove ones in blacklist since they failed before
    (car (sort l #'(lambda (a b) (< (clause-length a) (clause-length b))))))); Then we choose the one with shortest length

(defun try-resolute (a b)
  "Try to resolute two sentences"
  (cond ((opposite-p a b); if only a is not-clause
         (if (equal (cadr a) b) nil; if equal we are done
             (unify (cadr a) b))); or we try unification
        ((opposite-p b a)
         (if (equal a (cadr b)) nil
             (unify a (cadr b))))
        (t 'fail)))

(defun subs-var (theta clauses)
  (if (null theta)
      clauses
      (subs-var (cdr theta) ; recursively substitute every item in theta list
                (subst (cdr (car theta))
                       (car (car theta))
                       clauses
                       :test #'equal))))

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


;;====================
;; TEST CASES 
;;====================
;; All english description of the test cases are in class lectures

;; Test case A: Colonel West is a criminal
(defparameter *test-kb-a*  '((or (~ (american =x)) (~ (weapon =y)) (~ (sells =x =y =z)) (~ (hostile =z)) (criminal =x))
                           (american West)
                           (or (~ (missle =x)) (weapon =x))
                           (missle M1)
                           (or (~ (missle =x)) (~ (owns Foo =x)) (sells West =x Foo))
                           (owns Foo M1)
                           (or (~ (enemy =x America)) (hostile =x))
                           (enemy Foo America)))

(defparameter *test-target-a* '(~ (criminal West)))

;; Test case B: Smugglers and VIPs
(defparameter *test-kb-b* '((or (~ (e =x)) (v =x) (s (_f =x)))
                            (or (~ (e =x)) (v =x) (c (_f =x)))
                            (p d)
                            (e d)
                            (or (~ (s d =y)) (p =y))
                            (or (~ (p =z)) (~ (v =z)))))
(defparameter *test-target-b* '(or (~ (p =w)) (~ (c =w))))

;; Test case C: Scuba diver or water skier?
(defparameter *test-kb-c* '((or (ws =x) (sd =x))
                    (or (~ (ws =y)) (~ (likes =y Waves)))
                    (or (~ (ws =z)) (likes =z Warm))
                    (or (~ (likes Laura =w)) (~ (likes Jacob =w)))
                    (or (likes Jacob =w) (likes Laura =w))
                    (likes Jacob Warm)
                    (likes Jacob Waves)))
(defparameter *test-target-c* '(or (~ (sd =v)) (ws =v)))


(defun test ()
  "TEST ALL 3 CASES TOGETHER: IF get (T T T) then pass"
  (loop for result in
       (mapcar #'(lambda (x) (caddr (car (last x))))
               (list (atp *test-kb-a* *test-target-a*)
                     (atp *test-kb-b* *test-target-b*)
                     (atp *test-kb-c* *test-target-c*)))
       collect (not result)))
