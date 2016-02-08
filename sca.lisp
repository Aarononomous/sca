;;; Default globals
(defparameter *window* nil)
(defparameter *simulation* nil)
(defparameter *simulation-height* 24)
(defparameter *simulation-width*  40)
(defparameter *simulation-speed* 0.1)
(defparameter *simulation-title* "No simulation loaded")
(defparameter *simulation-description* "---")
(defparameter *active* 't) ; We enter in media res, ready to accept input
(defparameter *paused* 't) ; A non-existent simulation isn't run

(load "./packages.lisp")
(load "./screen.lisp")

(defun check-keyboard-interrupt ()
  "Break out of the loop, temporarily or permanently."
  (print "checking for interrupts from the keyboard")
  (let ((key (cl-charm:get-char)))
    (cond
      ((eq key #\P) (pause-simulation))
      ((eq key #\[) (pause-simulation))
      ((eq key #\L) (load-simulation))
      ((eq key #\S) (save-screenshot))
      ((eq key #\X) (exit-program))))

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

(defun update-simulation (description)
  "Given a string containing a simulation description (retrieved by load-simulation, probably), reads/evals it and replaces the current simulation."
  (let ((new-simulation (read-string description)))
    (if (is-acceptable description)
	(setf *simulation* new-simulation))))

(defun save-screenshot ()
  (print "saving screenshot"))

(defun update-simulation ()
  "Update the simulation to the next generation and display it on the screen"
  (print '(updating the simulation))
  (screen:clear-screen *window*))

(defun setup ()
  (setf *window* (cl-charms:initialize)))

(defun exit-program ()
  (print "exiting program")
  (cl-charm:finalize)
  (setf *active* nil))
  
(defun run-loop ()
  (if (not *paused*)
      (check-keyboard-interrupt)
      (update-simulation)))

;;; Run the program

;; (princ (cl-charms:window-dimensions *window*))
(while *active* (run-loop))
