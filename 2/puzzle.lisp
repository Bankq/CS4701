;; ==================================
;;  Problem dependent
;; ==================================
                                         
;;
;; A state of 8-puzzle problem can be viewed as
;;      1 2 3
;;      4 5 6
;;      7 8 0
;; where 0 is the placeholder
;; We represent the state using a list
;; '(0 1 2 3 4 5 6 7 8)
;; An action is represent as ("MOVE")
;; e.g. (right 8) indicates move 8 to its right,
;; '(0 1 2 3 4 5 6 7 8) --("right")--> '(1 0 2 3 4 5 6 7 8)


(defun 8-puzzle (state heuristic)
  (general-search state #'puzzle-successor #'puzzle-goalp :samep #'puzzle-samep :key heuristic))

(defparameter *puzzle-goal-state* '(0 1 2 3 4 5 6 7 8))

(defun puzzle-goalp (state)
  (puzzle-samep state *puzzle-goal-state*))

(defun puzzle-samep (state1 state2)
  (equal state1 state2))

(defun puzzle-successor (state)
  "generate legal move from one state
   (successor state) => (action new-state cost)"
  (let ((blank (position 0 state)))
    (remove-if #'null
               (list (when (> blank 2) (list "up" (move-up state) 1))
                     (when (< blank 6) (list "down" (move-down state) 1))
                     (when (> (mod blank 3) 0) (list "left" (move-left state) 1))
                     (when (< (mod blank 3) 2) (list "right" (move-right state) 1))))))

(defun move-up (state)
  (let ((blank (position 0 state))
        (result (copy-list state)))
    (rotatef (nth blank result) (nth (- blank 3) result))
    result))

(defun move-down (state)
  (let ((blank (position 0 state))
        (result (copy-list state)))
    (rotatef (nth blank result) (nth (+ blank 3) result))
    result))

(defun move-left (state)
  (let ((blank (position 0 state))
        (result (copy-list state)))
    (rotatef (nth blank result) (nth (- blank 1) result))
    result))

(defun move-right (state)
  (let ((blank (position 0 state))
        (result (copy-list state)))
    (rotatef (nth blank result) (nth (+ blank 1) result))
    result))

(defun misplaced (node)
  "Heuristic function. Count how many tiles are misplaced"
  (let ((state (node-state node))
        (g (node-path-cost node)))
    (+ g (count-misplaced state))))


(defun count-misplaced (state)
  (count-if-not #'(lambda (x) (= x (position x state))) state))

(defun manhattan (node)
  "Heuristic function. Count how many tiles are misplaced"
  (let ((state (node-state node))
        (g (node-path-cost node)))
    (+ g (manhattan-distance state))))

(defun manhattan-distance (state)
  (let  ((x-pos #'(lambda (e)
                    (car (list (floor e 3)))))
         (y-pos #'(lambda (e)
                    (mod e 3))))
    (reduce #'+ (mapcar #'(lambda (e) (+ (abs (- (funcall x-pos e) (funcall x-pos (position e state))))
                                              (abs (- (funcall y-pos e) (funcall y-pos (position e state))))))
                             state))))
(defun extracredit (node)
  (let ((state (node-state node))
        (g (node-path-cost node)))
    (+ g (max (count-misplaced state) (manhattan-distance state)))))

(defun random-case (&key (num 5) (depth 8))
  "Generate num random SOLVABLE state"
  (loop for i from 1 to num
       collect (generate-random-case :depth depth)))

(defun generate-random-case (&key (state *puzzle-goal-state*) (depth 10))
  "Generate one random case"
  (if (= depth 0)
      state
      (generate-random-case :state (let ((successors (puzzle-successor state)))
                                      (elt (mapcar #'(lambda (e) (cadr e)) successors) 
                                           (random (length successors))))
                                    :depth (1- depth))))
                                    
(defun test ()
  (loop for case in (random-case)
         collect (list (cadr (8-puzzle case #'manhattan))
                       (cadr (8-puzzle case #'misplaced)))))

;; ===================================
;;  General search 
;; ===================================

(defun general-search (initial-state successor goalp 
                       &key (samep #'eql) (enqueue #'enqueue-priority) (key #'identity))
  "General search routine"
  (let ((fringe (make-q :enqueue enqueue :key key)))
    (q-insert fringe (list (make-node :state initial-state)))
    (graph-search fringe nil successor goalp samep)))

(defun graph-search (fringe closed successor goalp samep)
  (unless (q-emptyp fringe)
      (let ((node (q-remove fringe)))
        (cond ((funcall goalp (node-state node))
               (list (action-sequence node) (node-depth node) (length closed)))
              ((member (node-state node) closed
                       :test samep :key #'node-state)
               (graph-search fringe closed successor goalp samep))
              (t (let ((successors (expand successor node)))
                   (graph-search (q-insert fringe successors)
                                 (cons node closed)
                                 successor goalp samep)))))))



(defun expand (successor node)
  (let ((triples (funcall successor (node-state node))))
    (mapcar (lambda (action-state-cost)
              (let ((action (car action-state-cost))
                    (state (cadr action-state-cost))
                    (cost (caddr action-state-cost)))
                (make-node :state state 
                           :parent node 
                           :action action 
                           :path-cost (+ (node-path-cost node) cost)
                           :depth (1+ (node-depth node)))))
            triples)))

(defun action-sequence (node &optional (actions nil))
  (if (node-parent node)
      (action-sequence (node-parent node)
                       (cons (node-action node) actions))
      actions))
;; ===================================
;;  Basic data structure
;; ===================================
(defstruct q
  (enqueue #'enqueue-FIFO)
  (key #'identity)
  (last nil)
  (elements nil))

(defun q-emptyp (q)
  "Return T is queue is empty"
  (= (length (q-elements q)) 0))

(defun q-front (q)
  "Return the element at the front of the queue"
  (elt (q-elements q) 0))

(defun q-remove (q)
  "Remove and returns the elements at the front of the queue."
  (if (listp (q-elements q))
      (pop (q-elements q))
      (heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
  "Inserts the items into the queue, according to
queue's enqueuing function. Returns the altered queue."
  (funcall (q-enqueue q) q items)
  q)

(defun enqueue-LIFO (q items)
  "Adds a list of items to the front of the queue"
  (setf (q-elements q) (nconc items (q-elements q)))
  items)

(defun enqueue-FIFO (q items)
  "Adds a list of items to the end of the queue"
  (if (q-emptyp q)
      (setf (q-elements q) items)
      (setf (cdr (q-last q)) items))
  (setf (q-last q) (last items))
  items)

(defun enqueue-priority (q items)
  "Inserts the items by priority of key values."
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  (mapc (lambda (item)
          (heap-insert (q-elements q) item (q-key q)))
        items)
  items)

(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i]
   may be larger than its children. If it is, moves heap[i] 
   down where it belongs."
  (unless (heap-leafp heap i)
    (let ((left-index (heap-left i))
          (right-index (heap-right i)))
      (let ((smaller-index
             (if (and (< right-index (length heap))
                      (< (heap-val heap right-index key)))
                 right-index
                 left-index)))
        (when (> (heap-val heap i key)
                 (heap-val heap smaller-index key))
          (rotatef (elt heap i)
                   (elt heap smaller-index))
          (heapify heap smaller-index key))))
    ))
    
   
(defun heap-pop (heap key)
  "Pops the best (lowest valued) item of the heap."
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Puts an item into a heap."
  (vector-push-extend nil heap)
  (setf (elt heap (heap-find-pos heap (1- (length heap))
                                 (funcall key item) key))
        item)
  )

(defun heap-find-pos (heap i val key)
  "Bubbles up from i to find position for val, moving items
   down in the process."
  (cond ((or (zerop i)
             (< (heap-val heap (heap-parent i) key) val))
         i)
        (t (setf (elt heap i) (elt heap (heap-parent i)))
           (heap-find-pos heap (heap-parent i) val key))
))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))


(defstruct node 
  state (parent nil) (action nil) (path-cost 0) (depth 0))




(provide 'puzzle)
