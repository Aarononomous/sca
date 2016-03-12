;;;; sca.lisp: A stochastic cellular automata simulator

(defpackage #:sca
  (:documentation "The SCA model.")
  (:use #:cl)
  (:export
   :run))

(in-package :sca)

;;; Load libraries
(ql:quickload "cl-charms") ; Curse library

; use colors for reversing the action bar
(load (merge-pathnames "colors.lisp" *load-truename*))
(load (merge-pathnames "model.lisp" *load-truename*)) ; Model code
(use-package :model)

;;; Parameters

;; Screen - these will be reset by cl-charms
(defparameter *height* 25) ;; screen height
(defparameter *width*  40) ;; screen height

;; Display and animation
(defparameter *break* nil) ;; break the main loop
(defparameter *modal* nil) ;; is a modal window on screen?
(defparameter *action-line* "[P]ause/Play [L]oad file [S]creenshot [I]nformation e[X]it") ; the default prompt line
(defparameter *speed* 500) ;; milliseconds per frame (minimum)

;; Model information
(defparameter *model-loaded* nil)
(defparameter *generation* 1)
(defparameter *states* '(empty " ")) ; property list

;;; Run the program

(defun run ()
  "Runs the simulation. The only exported function from sca.lisp."
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys charms:*standard-window*)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    ;; clear window
    (charms:clear-window charms:*standard-window*)
    ;; set params
    (setf (values *width* *height*)
	  (charms:window-dimensions charms:*standard-window*))
    ;; run the main loop
    (main-loop)
    ;; The simulation's ended - reset display for the next run
    (setf *break* nil
	  *modal* nil
	  *generation* 1)))

(defun main-loop ()
  "The main loop updates the screen and checks for input"
  (loop
     named main-loop
     with time = (get-internal-real-time)
     for key = (charms:get-char charms:*standard-window*
				:ignore-error t)
     do (progn
	  (when (> *speed* (- (get-internal-real-time) time))
	    (update-screen)
	    (update-model))
	  (check-keyboard-interrupt key)

	  ;; Exit program
	  (if *break*
	      (return-from main-loop))
	  
	  (setf time (get-internal-real-time)))))

(defun update-screen ()
  "Refreshes the screen: clears, redraws any loaded model and the action line."
  (charms:clear-window charms:*standard-window* :force-repaint t)
  (if *model-loaded*  
      (print-model))
  (display-action-line *action-line*)
  (charms:refresh-window charms:*standard-window*))

(defun update-model ()
  (if *model-loaded*
      (progn (model:update-world)
	     (incf *generation*))))

;;; Input (Controller)

(defun check-keyboard-interrupt (key)
  "Break out of the loop, temporarily or permanently."
  ;; TODO: make this a case statement, allow for capitals
  (cond
    ((eq key #\Escape) (escape-modal))
    ((eq key #\p) (pause-simulation))
    ((eq key #\l) (load-simulation))
    ((eq key #\i) (display-information))
    ((eq key #\s) (save-screenshot))
    ((eq key #\x) (exit-program))))

(defun escape-modal ()
  "Escape from a modal overlay"
  ;; TODO: escape from prompt as well?
  (setf *modal* nil)
  (remove-modal))

(defun pause-simulation ()
  "Pauses/unpauses the simulation"
  ;; TODO: this
  (display-modal "Paused/Unpaused"))

(defun load-simulation ()
  "Loads a new simulation"
  (unless (and *model-loaded*
	       (not (string= (prompt "A model is already loaded. Remove it? yes/no") "yes")))
    (progn
      (let ((file (prompt "Filename:")))
       	(if (probe-file file)
	    (progn
	      ;; if this fails don't blow up, but prompt a message
	      (ignore-errors
		(model:load-model file)
		(in-package :sca) ; return to this package
		(setf *model-loaded* t
		      *states* (model:get-states)))
	      
	      (if *model-loaded*
		  (prompt "~A is loaded. Hit return to continue.")
		  (prompt "~A didn't load properly. Hit return to continue." file)))
	    
	    (prompt "File ~A not found. Hit return to continue." file))))))

(defun save-screenshot ()
  "Saves the model as a string to a file."
  ;; TODO: this
  (display-modal "saving screenshot"))

(defun exit-program ()
  "Exits the program by setting the loop break variable to nil"
  ;; TODO: check to make sure this wasn't a mistake
  (unless (and *model-loaded*
	      (not (string= (prompt "A model is already loaded. Are you sure you want to exit? yes/no") "yes")))
   (setf *break* t)))

;;; Prompt Input (Another Controller)

(defun prompt (message &rest format-params)
  "Displays the prompt on the action line and returns the text
   that was entered. This is the most complicated function in the
   program; it contains its own input loop."
  (clear-action-line)
  (let ((input nil)
	(prompt-string (display-action-line message format-params)))
    (charms:move-cursor charms:*standard-window*
			(1+ (length prompt-string))
			(1- *height*))

    ;; TODO: disable echoing, print entered char to screen, capture backspace,
    ;; add backspace capabilities
    (charms:enable-echoing)
    (charms:disable-non-blocking-mode charms:*standard-window*)

    ;; read line and return it as a string
    (loop 
       named prompt-loop
       for c = (charms:get-char charms:*standard-window*)
       do (progn
	    (case c
	      ((nil) nil)
	      ;; TODO: backspace ASAP
	      (#\Newline (return-from prompt-loop))
	      (otherwise (setf input (append input (list c)))))
	    ))
    
    ;; reset input mode
    (charms:disable-echoing)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    ;; reset action line
    (clear-action-line)
    (display-action-line *action-line*)

    ;; return response as a string
    (coerce input 'string)))

;;; Output (View)
;;;
;;; The three main outputs --
;;; The modal window, for larger amounts of information
;;; The action line, at the bottom of the screen, for displaying
;;;   input prompts
;;; The simulation

(defmacro display-modal (message &rest format-params)
  "Displays the message on the center of the screen. Used by
   display-information, etc."
  ;; TODO: remove any other modal that may be on screen already
  ;; TODO: set *modal*
  `(let ((my-str (format nil ,message ,@format-params)))
     (charms:write-string-at-point
      charms:*standard-window*
      my-str
      0
      0)
     my-str))

(defun remove-modal ()
  ;; TODO: this
  )

(defmacro display-action-line (message &rest format-params)
  "Displays the message (the first 40 chars of it) on the action
   bar. Used by prompt, update-screen, etc."
  ;; TODO: clear action line first
  ;; TODO: trim to 40 chars? -- determine how many exactly
  `(let ((my-str (format nil ,message ,@format-params)))
     (charms:write-string-at-point
      charms:*standard-window*
      my-str
      0
      (1- *height*))
     my-str))

(defun clear-action-line ()
  "Clears the action line"
  (display-action-line (make-string (1- *width*)
				    :initial-element #\Space)))

(defun display-information ()
  "Displays a modal window with the model's information."
  ;; TODO: this, better
  (if *model-loaded*
      (display-modal "~a~%--------~%~A"
		     (model:get-title)
		     (model:get-description))
      (display-modal "~a~%--------~%~A"
		     "No model loaded"
		     "")))

(defun get-symbol (state)
  "Return a single printable character for the agent. If the
   symbol has already been associated with a printable glyph,
   return that. If not, return a space (' ')."
  (getf *states* state " "))

(defun print-model (&optional (stream t))
  "Prints the world to the stream. Used for all output."
  (let ((model (model:get-world)))
    (dotimes (row (array-dimension model 0))
      (dotimes (col (array-dimension model 1))
	(charms:write-string-at-point charms:*standard-window*
				    (get-symbol (aref model row col))
				    (* 2 col)
				    row)))))
