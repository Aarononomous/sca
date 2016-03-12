;;;; colors.lisp
;;;;
;;;; Helpers for ANSI colorization of the model agents
;;;; use like (blue "*") to get a blue star, e.g.
;;;; note that these can be nested

;; Colors

(defun colorize (ansi-code str)
  (format nil "~C[~Am~A~C[39m" #\Escape ansi-code str #\Escape))

(defun black (str) (colorize "38;5;0" str))

(defun red (str) (colorize "38;5;1" str))

(defun green (str) (colorize "38;5;2" str))

(defun brown (str) (colorize "38;5;3" str))

(defun blue (str) (colorize "38;5;4" str))

(defun purple (str) (colorize "38;5;5" str))

(defun cyan (str) (colorize "38;5;6" str))

(defun gray (str) (colorize "38;5;7" str))

(defun dark-gray (str) (colorize "38;5;8" str))

(defun light-red (str) (colorize "38;5;9" str))

(defun light-green (str) (colorize "38;5;10" str))

(defun yellow (str) (colorize "38;5;11" str))

(defun light-blue (str) (colorize "38;5;12" str))

(defun light-purple (str) (colorize "38;5;13" str))

(defun light-cyan (str) (colorize "38;5;14" str))

(defun white (str) (colorize "38;5;15" str))

;; Other formatting operations

(defun negative (str)
  (format nil "~C[7m~A~C[27m" #\Escape str #\Escape))

(defun bold (str)
  (format nil "~C[1m~A~C[22m" #\Escape str #\Escape))
