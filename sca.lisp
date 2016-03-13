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
(defparameter *playing* nil) ;; play/pause
(defparameter *modal* nil) ;; the message in the modal window
(defparameter *default-prompt* "[P]ause/Play [L]oad file [S]creenshot [I]nformation e[X]it")
(defparameter *action-line* *default-prompt*)
(defparameter *speed* 100) ;; milliseconds per frame (minimum)

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

    ;; colors - not operational yet
    ;; (charms/ll:start-color)
    ;; (charms/ll:init-pair 1 charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)

    ;; clear window
    (charms:clear-window charms:*standard-window*)
    ;; set params
    (setf (values *width* *height*)
	  (charms:window-dimensions charms:*standard-window*))
    ;; run the main loop
    (main-loop)
    ;; The simulation's ended - reset display for the next run
    (setf *break* nil
	  *generation* 1)))

(defun main-loop ()
  "The main loop updates the screen and checks for input"
  (loop
     named main-loop
     with tick = (get-internal-real-time)
     for key = (charms:get-char charms:*standard-window*
				:ignore-error t)
     and time = (get-internal-real-time)
     do (progn
	  ;; Redraw the screen
	  (charms:clear-window charms:*standard-window* :force-repaint t)
	  
	  (if *model-loaded*
	      (print-model))

	  (if *modal*
	      (display-modal *modal*))
	  
	  (if (and *playing*
		   (< *speed* (- time tick)))
	      (progn
		(update-model)
		(setf tick time)))

	  ;; Check for input
	  (check-keyboard-interrupt key)

	  (clear-action-line)
	  (display-action-line *action-line*)
	  
	  (charms:refresh-window charms:*standard-window*)

	  ;; Exit program
	  (if *break*
	      (return-from main-loop)))))

(defun update-model ()
  (if *model-loaded*
      (progn (model:update-world)
	     (incf *generation*))))

;;; Input (Controller)

(defun check-keyboard-interrupt (key)
  "Break out of the loop, temporarily or permanently."
  ;; TODO: make this a case statement, allow for capitals
  (case key
    ((nil) nil)
    ((#\Escape) (setf *modal* nil)) ; escape the modal window
    ((#\p #\P) (pause-simulation))
    ((#\l #\L) (load-simulation))
    ((#\i #\I) (display-information))
    ((#\s #\S) (save-screenshot))
    ((#\x #\X) (exit-program))))

(defun pause-simulation ()
  "Pauses/unpauses the simulation"
  ;; TODO: this
  (if *playing*
      (progn (setf *playing* nil)
	     (setf *action-line* "Paused"))
      (progn (setf *playing* t)
	     (setf *action-line* *default-prompt*))))

(defun load-simulation ()
  "Loads a new simulation"
  (unless (and *model-loaded*
	       (not (string= (prompt "A model is already loaded. Remove it? yes/no:") "yes")))
    (progn
      (let ((file (prompt "Filename:")))
       	(if (probe-file file)
	    (progn
	      ;; if this fails don't blow up, but prompt a message
	      (ignore-errors
		(model:load-model file
				  :height (1- *height*)
				  :width (floor *width* 2))
		(in-package :sca) ; return to this package
		(setf *model-loaded* t
		      *states* (model:get-states)))
	      
	      (if *model-loaded*
		  (prompt "~A is loaded. Hit return to continue." file)
		  (prompt "~A didn't load properly. Hit return to continue." file)))
	    
	    (prompt "File ~A not found. Hit return to continue." file))))))

(defun display-information ()
  "Displays a modal window with the model's information."
  ;; TODO: this, better
  (let ((divider (make-string (1- *width*)
			      :initial-element #\-)))
    (setf *modal* (format nil  "~A~%~A~%~A~%~A~%~A"
			  divider
			  (model:get-title)
			  divider
			  (model:get-description)
			  divider))))

(defun save-screenshot ()
  "Saves the model as a string to a file."
  ;; TODO: this
  (let ((model (model:get-world))
	(file (prompt "Filename:")))
    (with-open-file (filestream
		     (merge-pathnames file *load-truename*)
		     :direction :output
		     :if-exists :rename)
      (dotimes (row (array-dimension model 0))
	(dotimes (col (array-dimension model 1))
	  do (format filestream "~A " (get-symbol (aref model row col))))
	(format filestream "~%")))))


(defun exit-program ()
  "Exits the program by setting the loop break variable to nil"
  (unless (and *model-loaded*
	       (not (string= (prompt "A model is already loaded. Are you sure you want to exit? yes/no:") "yes")))
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
    (charms:disable-non-blocking-mode charms:*standard-window*)

    ;; read line and return it as a string
    (loop 
       named prompt-loop
       for c = (charms:get-char charms:*standard-window*)
       do (case c
	    ((nil) nil)
	    ;; TODO: backspace ASAP
	    ((#\Rubout) (unless (null input)
			  (progn
			    (charms:move-cursor-left
			     charms:*standard-window*)
			    (charms:write-char-at-cursor
			     charms:*standard-window*
			     #\Space)
			    (charms:move-cursor-left
			     charms:*standard-window*)
			    (setf input (butlast input)))))
	    ((#\Newline) (return-from prompt-loop))
	    (otherwise (progn
			 (charms:write-char-at-cursor
			  charms:*standard-window*
			  c)
			 (setf input
			       (append input (list c)))))))
    
    ;; reset input mode
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

(defun display-modal (message)
  "Displays the message on the center of the screen. Used by
   display-information, etc."
  (charms:write-string-at-point
   charms:*standard-window*
   message
   0
   0))

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

(defun get-symbol (state)
  "Return a single printable character for the agent. If the
   symbol has already been associated with a printable glyph,
   return that. If not, return a space (' ')."
  (getf *states* state " "))

(defun print-model (&optional (stream t))
  "Prints the world to the stream. Used for all output."
  (let ((model (model:get-world)))
    (dotimes (row (min (array-dimension model 0)
		       (1- *height*)))
      (dotimes (col (min (array-dimension model 1)
			 (floor *width* 2)))
	(charms:write-string-at-point charms:*standard-window*
				      (get-symbol (aref model row col))
				      (* 2 col)
				      row)))))
