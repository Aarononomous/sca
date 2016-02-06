(defpackage :com.aaron-jacobson.setup
  (:use :common-lisp)
  (:nicknames :setup)
  (:export :screen))

(in-package setup)

(defun screen ()
  "Set up the screen with ncurses"
  (print "setting up the screen")
  nil)
