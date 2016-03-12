;;;; sca.lisp: A stochastic cellular automata simulator

(defpackage #:sca
  (:documentation "The SCA model.")
  (:use #:cl)
  (:export
   :run))

(in-package :sca)

;;; Load libraries
(ql:quickload "cl-charms") ; Curse library

;; (load (merge-pathnames "packages.lisp" *load-truename*)) ; set up packages
(load (merge-pathnames "model.lisp" *load-truename*)) ; Model code
(use-package :model)

;;; Default parameters
;; Screen
(defparameter *height* 25) ;; screen height
(defparameter *width*  40) ;; screen height

;; Display
(defparameter *break* nil) ;; break the main loop
(defparameter *modal* nil) ;; is a modal window on screen?
(defparameter *action-line* "[P]ause/Play [L]oad file [S]creenshot [I]nformation e[X]it") ; the default prompt line
(defparameter *speed* 100) ;; milliseconds per frame (min)

;; Model information
(defparameter *model-loaded* nil)
(defparameter *generation* 1) ;; the generation of the model
(defparameter *states* '(('empty . "  ")))


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
  (setf *modal* nil)
  (remove-modal))

(defun pause-simulation ()
  (display-modal "Paused/Unpaused"))

(defun load-simulation ()
  "Loads a new simulation"
  ;; TODO: some way of cancelling this.
  ;; TODO: prompt if there's already a simulation running
  (unless (and *model-loaded*
	       (not (string= (prompt "There's already a model loaded. Remove it? yes/no") "yes")))
    (progn
      (display-modal "Loading????")
      (let ((file (prompt "Filename:")))
       	(if (probe-file file)
	    (progn (ignore-errors ; if this fails, prompt a message
		     (model:load-model file)
		     (in-package :sca) ; return to this package
		     (setf *model-loaded* t
			   *states* (model:get-states)))
;		   (in-package :
		   (if *model-loaded*
		       (progn (display-modal "~A is loaded. 🗿" file)
			      (prompt "Hit return")
			      (display-modal "States: ~A" *states*)
			      (prompt ""))
		       
		       (prompt "~A didn't load properly. Hit return to continue." file)))
	    
	    (prompt "File ~A not found. Hit return to continue." file))))))

(defun display-information ()
  (if *model-loaded*
      (display-modal "~a~%--------~%~A"
		     (model:get-title)
		     (model:get-description))
      (display-modal "~a~%--------~%~A"
		     "No model loaded"
		     "")))

(defun save-screenshot ()
  ;; TODO: this
  (display-modal "saving screenshot"))

(defmacro display-modal (message &rest format-params)
  "Displays the message on the center of the screen"
  ;; TODO: remove any other modal that may be on screen already
  ;; TODO: pause, set *modal*
  `(let ((my-str (format nil ,message ,@format-params)))
     (charms:write-string-at-point
      charms:*standard-window*
      my-str
      0
      0)
     my-str))

(defmacro display-action-line (message &rest format-params)
  "Displays the message (the first 40 chars of it) on the action bar"
  ;; TODO: clear action line first
  ;; TODO: trim to 40 chars? -- determine how many exactly
  `(let ((my-str (format nil ,message ,@format-params)))
     (charms:write-string-at-point
      charms:*standard-window*
      my-str
      0
      (1- *height*))
     my-str))

(defun prompt (message &rest format-params)
  "Displays the prompt on the action line and returns the entered text"
  ;;; TODO: reverse line coloring?
  ;; add bolding to letters instead of brackets?
  ;; need to do this to clear-line as well
  ;; [7m inverse on; reverses foreground & background colors
  ;; [1m bold on
  ;; [22m bold off (see below)
  (clear-action-line)
  (let ((input nil)
	(prompt-string (display-action-line message format-params)))
    (charms:move-cursor charms:*standard-window*
			(1+ (length prompt-string))
			(1- *height*))

    ;; set input mode in charms
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
	      (#\Newline (return-from prompt-loop))
	      (otherwise (setf input (append input (list c)))))
					;(display-modal (string response))
	    ))
    
    ;; reset input mode
    (charms:disable-echoing)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    ;; reset action line
    (clear-action-line)
    (display-action-line *action-line*)

    ;; return response as a string
    (coerce input 'string)))

(defun clear-action-line ()
  (display-action-line (make-string (1- *width*) :initial-element #\Space)))

(defun exit-program ()
  "Exits the program by setting the loop break variable to nil"
  ;; TODO: check to make sure this wasn't a mistake
  (setf *break* t))

(defun remove-modal ()
  ;; TODO: this
  )

(defun get-symbol (symbol)
  "Return a single printable character for the symbol. If the symbol
   has already been associated with a printable glyph, return that.
   If not, assume it's empty and return two spaces (\"  \")."
  (let ((k-v-pair (assoc symbol *states*)))
    (if k-v-pair
	(cdr k-v-pair)
	"  "))) ; for debugging

(defun print-model (&optional (stream t))
  "Prints the world to the stream. Used for all output."
  (let ((model (model:get-world)))
    (dotimes (row (array-dimension model 0))
      (dotimes (col (array-dimension model 1))
	(charms:write-string-at-point charms:*standard-window*
				    (get-symbol (aref model row col))
				    (* 2 col)
				    row)))))

(defun draw-simulation ()
  (if *model-loaded*  
      (print-model)))

(defun update-model ()
  (if *model-loaded*
      (progn (model:update-world)
	     (incf *generation*))))

(defun update-screen ()
  "Refreshes the screen: clears, redraws any loaded model and the action line."
  (charms:clear-window charms:*standard-window* :force-repaint t)
  (draw-simulation)
  (display-action-line *action-line*)
  (charms:refresh-window charms:*standard-window*))

(defun main-loop ()
  "The main loop updates the screen and checks for any input"
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

;;; Run the program

(defun run ()
  "Once this is working, I'll remove the function call. I think?"
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
    ;;
    (main-loop)
    ;; The simulation's ending - reset parameters for another run
    (setf *break* nil
	  *modal* nil
	  *generation* 1)))
