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

;; Imports
(load (merge-pathnames "colors.lisp" *load-truename*))

;;; Parameters

(defparameter *world* nil)
(defparameter *agents* '(empty " ")) ; property list
(defparameter *transition-table* nil)
(defparameter *title* "No simulation loaded")
(defparameter *description* "---")

;;; Exported functions: information about the model
;;; and updating it

(defun load-model (file)
  "Loads the model. This just runs 'file,' so any lisp
   code can be inside. The additional syntax, 'world,'
   'trans,' etc. are macros defined below."
  (in-package :model) ; use unexported functions
  (load file :verbose nil))

(defun get-title ()
  "Returns the title of the model"
  *title*)

(defun get-description ()
  "Returns the description of the model"
  *description*)

(defun get-states ()
  "Returns an property list of agent symbols to one-letter
   string agent representations. (If ANSI color codes are used,
   strings may be longer than one character, but only one
   character should be displayed.)"
  *agents*)

(defun get-world ()
  "Returns the world, a 2-dimensional array of symbols."
  *world*)

(defun update-world ()
  "Updates every cell in *world*."
  ;; TODO: in-package model?
  (let ((world+1 (make-array (array-dimensions *world*))))
    (dotimes (row (array-dimension *world* 0))
      (dotimes (col (array-dimension *world* 1))
	(setf (aref world+1 row col) (next-state row col))))

    (setf *world* world+1)))

;;; Updating the world

(defun get-transitions (state)
  "Gets all the transitions for a state. Because this is a non-
   deterministic model, there may be more than one possible
   transition for a given state."
  (loop for s in *transition-table*
     when (eq state (first s))
     collect s))

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

;;; SCA file syntax

(defun world (&optional &key (dimensions '(24 40))
			  (proportions '())
			  (start-configuration '()))
  "Creates a new world:
   dimensions is the '(height width) of the model.
   proportions is a list of '(agent proportion)s in the model's
   initial state.
   start-configuration is a 2-d array containing the initial
   state. If this is set, then any passed dimensions or
   proportions are ignored."
  (if start-configuration
      (setf *world* start-configuration)
      ;; otherwise
      (progn
	;; set dims
	(setf *world* (make-array dimensions
				  :initial-element 'empty))
	;; TODO: set initial proportions
	)))

(defun title (new-title)
  "Set the title of the model to the title from the file."
  (setf *title* new-title))

(defun description (new-description)
  "Set the model description to the one from the file."
  (setf *description* new-description))

(defun state (agent string)
  "Adds (agent . string) to *agents*"
  ;; TODO: setf-able???
  ;; (setf (getf *agents* agent) string)
  (nconc *agents* (list agent string)))

(defun trans (current-state transition-probability new-state)
  "Add this transition to the transition table. Transitions are
   loaded in the order in which they're written."
  (setf *transition-table*
	(append *transition-table*
		(list (list current-state transition-probability new-state)))))

;;; Neighbor functions

(defun turns-into ()
  "With a probability of 1, this cell will turn into the next."
  1.0)

(defun neighbors-of-type (agent-type)
  "The number of agent-type agents in the eight neighboring cells"
  (loop for neighbor in *current-neighbors*
     count (eq neighbor agent-type)))

(defun neighbor (agent)
  (if (> (neighbors-of-type agent) 0) 1.0 0.0))

(defun neighbor= (agent num)
  (if (= (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor< (agent num)
  (if (< (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor> (agent num)
  (if (> (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor<= (agent num)
  (if (<= (neighbors-of-type agent) num) 1.0 0.0))

(defun neighbor>= (agent num)
  (if (>= (neighbors-of-type agent) num) 1.0 0.0))

;;; Output
;;;
;;; Use for debugging

(defun get-symbol (agent)
  "Return a single printable character for the agent. If the
   symbol has already been associated with a printable glyph,
   return that. If not, return a space (' ')."
  (getf *agents* agent " "))

(defun print-world (&optional (stream t))
  "Prints the world line by line."
  (dotimes (row (array-dimension *world* 0))
    (dotimes (col (array-dimension *world* 1))
      do (format stream "~A " (get-symbol (aref *world* row col))))
    (format stream "~%")))
