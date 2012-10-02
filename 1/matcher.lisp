;;===============================================================
;;                     Pattern Macher
;;    W4701 Artificial Intelligence Homework 1, Fall 2012
;;    Author: Hang Qian
;;    UNI: hq2124
;;    Contact: hq2124(at)columbia.edu
;;     
;;    The description of the usage and functionality can be
;;    found at https://github.com/Bankq/CS4701/blob/master/1/instruction.txt
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

;;
;; The top function:(match pattern expression)
;; a binding-list is a vector of matching pairs
;; it is maintained within the running time 
(defun match (pat exp)
  (let ((binding-list (make-array 10 :fill-pointer 0 :adjustable t)))
    (catch 'fail (do-match pat exp binding-list))))

;;
;; do-match does the actual matching process by
;; calling list-match function because the pattern
;; and expression should be both list.
;; It will throw a 'fail flag if the list-match
;; returns nil and it will be catched by the top
;; function.
(defun do-match (pat exp bdl)
  (if (list-match pat exp bdl)
      (to-list bdl)
      (throw 'fail nil)))

;;
;; atom-match is called when the pattern is an atom.
;; It will call different routine based on the predicateds.
;;
(defun atom-match (pat exp bdl)
  (cond ((not (atom pat)) nil)
        ((is-pattern pat #\=) (equal-pattern-match pat exp bdl))
        ((is-pattern pat #\!) (not-equal-pattern-match pat exp bdl))
        ((is-pattern pat #\>) (compare-pattern-match #'> pat exp bdl))
        ((is-pattern pat #\<) (compare-pattern-match #'< pat exp bdl))
        (t (equal pat exp))))
;;
;; list-match is called when the pattern is a list.
;; It calls itself recursively on each item it contains.
;; 
(defun list-match (pat exp bdl)
  (cond ((null pat) (if (null exp) t nil))
        ;;can be omitted if the outer calling is legal
        ((atom pat) (atom-match pat exp bdl))
        ;;Need to test if pattern is formed as (& ...)
        ;;In this case, it's a single pattern.
        ((is-pattern-any pat) (any-pattern-match pat exp bdl))
        ((/= (length pat) (length exp)) nil)
        (t (loop for pat-item in pat
              for exp-item in exp
                ;;Test matcher recursively
              always (list-match pat-item exp-item bdl)))))

(defun is-pattern-any (pat)
  (equal "&" (string (car pat))))
  
;;
;; Test pat if it is predicated by pre
;;
(defun is-pattern (pat pre)
  (and (symbolp pat) (equal pre (char (string pat) 0))))
  
(defun any-pattern-match (pat exp bdl)
  (let ((rules (cdr pat)))
    (if (null rules) 
        t ;;(&) can match any thing
        ;; the exp need to be matched by all the patterns after &
        (loop for rule in rules
             always (list-match rule exp bdl)))))
;;
;; =x pattern routine
;;
(defun equal-pattern-match (pat exp bdl)
  (if (is-bind-exist pat bdl)
      (equal (find-binding pat bdl) exp)
        ;; The only case we add a new binding pair.
      (insert pat exp bdl)))
;;
;; !x pattern routine
;;
(defun not-equal-pattern-match (pat exp bdl)
  (if (is-bind-exist (fmt pat) bdl)
      (not (equal exp (find-binding (fmt pat) bdl)))
      nil))
;;
;; >x <x pattern routine
;; func is #'> or #'<
(defun compare-pattern-match (func pat exp bdl)
  (if (is-bind-exist (fmt pat) bdl)
      (let ((prev (find-binding (fmt pat) bdl)))
        (if (and (numberp prev) (numberp exp))
            (funcall func exp prev)))
      nil))

;;
;; check if pat has already been bound
;;
(defun is-bind-exist (pat bdl)
  (some #'(lambda(x) (equal (car x) pat)) bdl))

;;
;; return the bound value of pat in bdl
;;
(defun find-binding (pat bdl)
  (if (is-bind-exist pat bdl)
      (cdr (find pat bdl :key #'first))
      nil))

;;
;; Turn symbol *x to =x.
;; Because we store the bindings as
;; ((=x . A) ... )
;;
(defun fmt (pat)
  (intern (concatenate 'string "=" (string-upcase (subseq (string pat) 1)))))

(defun insert (pat exp bdl)
  (vector-push-extend (cons pat exp) bdl) 
  t)

;;
;; Turn the maintained array to a list
;; If it's empty ,return t
(defun to-list (a)
  (let ((l '())) 
    (every #'(lambda (x) (setq l (cons x l))) a)
    (if (equal 0 (length l)) t l)))

;;
;; The test cases
;;
(defun match-test ()
  (loop for case in (list (match '(a b c) '(a b c))
                          (not (match '(a b) '(c d)))
                          (equal (match '(a =x b) '(a z b)) 
                                 '((=X . Z)))
                          (equal (match '(=x =y) '(a b)) 
                                 '((=Y . B) (=X . A)))
                          (not (match '(=x !x) '((a b) (a b))))
                          (not (match '(!x =x) '(a a)))
                          (equal (match '(=x =y <x >y) '(5 7 3 10)) 
                                 '((=Y . 7) (=X . 5)))
                          (equal (match '(=x =y (& <x >y =z)) '(5 1 3)) 
                                 '((=Z . 3) (=Y . 1) (=X . 5)))
                          (equal (match '(& =x =y) '(a b c))
                                 '((=Y A B C) (=X A B C)))
                          (equal (match '(a (& =x =y (& =z =w))) '(a b))
                                 '((=W . B) (=Z . B) (=Y . B) (=X . B)))
                          )
       always case))

