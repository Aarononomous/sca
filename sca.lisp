;;; Default globals
(defparameter *window* nil)
(defparameter *world* nil)
(defparameter *agents* '(('empty . " ")))
(defparameter *transition-table* nil)
(defparameter *speed* 0.1)
(defparameter *title* "No simulation loaded")
(defparameter *description* "---")
(defparameter *active* 't) ; We enter in media res, ready to accept input
(defparameter *paused* 't) ; A non-existent simulation isn't run
(defparameter empty nil)

(ql:quickload "cl-charms")
(load (merge-pathnames "packages.lisp" *load-truename*))
(load (merge-pathnames "screen.lisp" *load-truename*))

(defun check-keyboard-interrupt ()
  "Break out of the loop, temporarily or permanently."
  (print "checking for interrupts from the keyboard")
  (let ((key (charms:get-char)))
    (cond
      ((eq key #\P) (pause-simulation))
      ((eq key #\[) (pause-simulation)) ;; TODO: escape key
      ((eq key #\L) (load-simulation))
      ((eq key #\S) (save-screenshot))
      ((eq key #\X) (exit-program)))))

(defun pause-simulation ()
  (if *paused*
      (print "pausing")
      (print "playing"))
  (setf *paused* (not *paused*)))

(defun load-simulation ()
  ;; TODO: some way of cancelling this.
  (setf *paused* 't)
  (print "loading simulation")
  (let ((file (prompt "Filename: ")))
    (if (is-file file)
	(update-simulation (read-lines file))
	(alert "File not found!")))
  (load-simulation))

(defun save-screenshot ()
  (print "saving screenshot"))

(defun update-simulation ()
  "Update the simulation to the next generation and display it on the screen"
  (print '(updating the simulation))
  (screen:clear-screen *window*))

(defun setup ()
  (setf *window* (charms:initialize)))

(defun exit-program ()
  (print "exiting program")
  (charms:finalize)
  (setf *active* nil))

(defun run-loop ()
  (if (not *paused*)
      (check-keyboard-interrupt)
      (update-simulation)))

;;; Run the program

;;; (princ (charms:window-dimensions *window*))
;;;(while *active* (run-loop))

;;; Loading a new simulation


(defun title (new-title)
  (setf *title* new-title))

(defun description (new-description)
  (setf *simulation-description new-description))

(defun cells (alists)
  (setf *agents* alists))

(defun world (&optional &key (dimensions '(24 40))
			  (proportions '())
			  (unicode t)
			  (start-configuration '()))

  (if start-configuration
      ;; set configuration
      (setf *world* start-configuration)
      ;; else set dims and any initial proportions
      (setf *world* (make-array dimensions
				:initial-element 'empty))
      ;; TODO: set initial proportions

      ))

(defun trans (current-state transition-probability new-state)
  "Add this transition to the transition table."
  (push (list current-state . (transition-probability new-state))
	*transition-table*))

(defun get-transitions (state)
    (loop for s in *transition-table*
       when (eq state (first s))
       collect s))

(defun get-symbol (symbol)
  "Return a single printable character for the symbol. If the symbol
   has already been associated with a printable glyph, return that.
   If not, return a space (\" \")."
  (let ((k-v-pair (assoc symbol *agents*)))
    (if k-v-pair
	(cdr k-v-pair)
	" ")))

(defun print-world ()
  "Prints the world to stdout."
  (dotimes (row (array-dimension *world* 0))
    (dotimes (col (array-dimension *world* 1))
      do (format t "~A " (get-symbol (aref *world* row col))))
    (format t "~%")))

(defun update-world ()
  "Performs one generation of updates to the world."
  (let ((world+1 (make-array (array-dimensions *world*))))
    
    (dotimes (row (array-dimension *world* 0))
      (dotimes (col (array-dimension *world* 1))
	(setf (aref world+1 row col) (next-state row col))))

    (setf *world* world+1)))

(defun next-state (row col)
  "The next state of the cell in *world* at row, col."
  (let ((neighbors (get-neighbors *world* row col))))
  ;;; TODO: go through trans table
  (aref *world* row col))

(defun get-neighbors (array row col)
  "A list of the neighbors of the cell at row, col."
  (loop for offset in '((-1 -1) (-1 0) (-1 1)
			(0  -1)        (0  1)
			(1  -1) (1  0) (1  1))
     when (array-in-bounds-p array (+ row (first offset)) (+ col (second offset)))
     collect (aref array (+ row (first offset)) (+ col (second offset)))))

;;; helper functions

(defun turns-into ()
  "With a probability of 1, this cell will turn into the next."
  1.0)

(defun neighbors-of-type (agent)
  (loop for neighbor in *current-neighbors*
     count (eq neighbor agent)))

(defun neighbor (agent)
  "Does the current cell have a neighbor of type agent?"
  (if (> (neighbors-of-type agent) 0) 1 0))

(defun neighbor= (agent num)
  "Does the current cell have exactly num neighbors of type agent?"
  (if (= (neighbors-of-type agent) num) 1 0))

(defun neighbor< (agent num)
  "Does the current cell have fewer than num neighbors of type agent?"
  (if (< (neighbors-of-type agent) num) 1 0))

(defun neighbor> (agent num)
  "Does the current cell have more than num neighbors of type agent?"
  (if (> (neighbors-of-type agent) num) 1 0))

(defun neighbor<= (agent num)
  "Does the current cell have no more than num neighbors of type agent?"
  (if (<= (neighbors-of-type agent) num) 1 0))

(defun neighbor>= (agent num)
  "Does the current cell have exactly num neighbors of type agent?"
  (if (>= (neighbors-of-type agent) num) 1 0))

