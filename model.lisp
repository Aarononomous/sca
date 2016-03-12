;;;; model.lisp
;;;;
;;;; Contains code for loading, managing, and updating stochastic
;;;; cellular automata. Use with sca.lisp (which loads this) for a
;;;; display and controller

(defpackage #:model
  (:documentation "Package containing an interface to the SCA model.")
  (:use #:cl)
  (:export
   :load-model           
   :get-title            
   :get-description      
   :get-states
   :get-world
   :update-world
   ))

(in-package #:model)



;;; Parameters

(defparameter *world* nil)
(defparameter *agents* '(('empty . "  "))) ; two spaces
(defparameter *transition-table* nil)
(defparameter *title* "No simulation loaded")
(defparameter *description* "---")

(defun load-model (file)
  "Loads the model from the named file"
  (in-package :model)
  (load file :verbose nil))
  
(defun world (&optional &key (dimensions '(24 40))
			  (proportions '())
			  (start-configuration '()))

  (if start-configuration
      ;; set configuration
      (setf *world* start-configuration)
      ;; else set dims and any initial proportions
      (setf *world* (make-array dimensions
				:initial-element 'empty))
      ;; TODO: set initial proportions

      ))

(defun get-world ()
  *world*)

(defun title (new-title)
  (setf *title* new-title))

(defun get-title ()
  *title*)

(defun description (new-description)
  (setf *description* new-description))

(defun get-description ()
  *description*)

(defun states (alists)
  ;; TODO: if I want ansi escape codes, I need to interpret the states
  ;; before setting up the alist. This is something that can be done
  ;; while macro-fying this
  ;;  (format t "~C[0;31m***" \esc)  
  (setf *agents* alists))

(defun get-states ()
  *agents*)

(defun trans (current-state transition-probability new-state)
  "Add this transition to the transition table. Transitions are loaded
   in the order they're written in."
  (setf *transition-table*
	(append *transition-table*
		(list (list current-state transition-probability new-state)))))

(defun get-transitions (state)
    (loop for s in *transition-table*
       when (eq state (first s))
       collect s))

(defun get-symbol (symbol)
  "Return a single printable character for the symbol. If the symbol
   has already been associated with a printable glyph, return that.
   If not, return two spaces (\"  \")."
  (let ((k-v-pair (assoc symbol *agents*)))
    (if k-v-pair
	(cdr k-v-pair)
	"  ")))

(defun print-world (&optional (stream t))
  "Prints the world to the stream. Used for all output."
  (dotimes (row (array-dimension *world* 0))
    (dotimes (col (array-dimension *world* 1))
      do (format stream "~A " (get-symbol (aref *world* row col))))
    (format stream "~%")))

(defun update-world ()
  "Sets *world* to its next generation."
  (let ((world+1 (make-array (array-dimensions *world*))))
    (dotimes (row (array-dimension *world* 0))
      (dotimes (col (array-dimension *world* 1))
	(setf (aref world+1 row col) (next-state row col))))

    (setf *world* world+1)))

(defun next-state (row col)
  "The next state of the cell in *world* at row, col."
  (let ((current-state (aref *world* row col))
	(neighbors (get-neighbors *world* row col))
	(transitions (get-transitions (aref *world* row col))))    
    (setf *current-neighbors* neighbors)
    ;; TODO: is there a better way to have the default be the same state?
    (loop for transition in (append transitions
				    (list (list current-state #'turns-into current-state)))
       when (< (random 1.0) (funcall (second transition)))
       return  (third transition))))

(defun get-neighbors (array row col)
  "A list of the neighbors of the cell at row, col."
  (loop for offset in '((-1 -1) (-1 0) (-1 1)
			(0  -1)        (0  1)
			(1  -1) (1  0) (1  1))
     when (array-in-bounds-p array (+ row (first offset)) (+ col (second offset)))
     collect (aref array (+ row (first offset)) (+ col (second offset)))))

;;; Helper Functions

;; Helpers for neighbors

(defun turns-into ()
  "With a probability of 1, this cell will turn into the next."
  1.0)

(defun neighbors-of-type (agent)
  (loop for neighbor in *current-neighbors*
     count (eq neighbor agent)))

(defun neighbor (agent)
  "Does the current cell have a neighbor of type agent?"
  (if (> (neighbors-of-type agent) 0) 1.0 0.0))

(defun neighbor= (agent num)
  "Does the current cell have exactly num neighbors of type agent?"
  (if (= (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor< (agent num)
  "Does the current cell have fewer than num neighbors of type agent?"
  (if (< (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor> (agent num)
  "Does the current cell have more than num neighbors of type agent?"
  (if (> (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor<= (agent num)
  "Does the current cell have no more than num neighbors of type agent?"
  (if (<= (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor>= (agent num)
  "Does the current cell have exactly num neighbors of type agent?"
  (if (>= (neighbors-of-type agent) num) 1.0 0.0))
