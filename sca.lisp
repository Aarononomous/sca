;;; Default globals
(defparameter *simulation-height* 24)
(defparameter *simulation-width*  40)
(defparameter *simulation-speed* 0.1)
(defparameter *simulation-title* "No simulation loaded")
(defparameter *simulation-description* "---")

(load "./setup.lisp")

(defun setup ()
  "Set up the screen with ncurses"
  (print '(setting up the screen)))

(defun check-keyboard-interrupt ()
  "Break out of the loop, temporarily or permanently."
  (print '(checking for interrupts from the keyboard)))

(defun update-simulation ()
  "Update the simulation to the next generation and display it on the screen"
  (print '(updating the simulation)))

(defun run-loop ()
  (loop
    (check-keyboard-interrupt)
    (update-simulation)))

;;; Run the program
(setup)
(run-loop)
