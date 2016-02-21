(ql:quickload "cl-charms")

;;; A test of the cl-charms library. Can I get it to work?

(defun clock ()
  "Clears the terminal, then displays a clock for 10 seconds in the top left."
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    
       (loop
	  with start = (get-universal-time)
	  do (progn
	       ;; clear the window
	       (charms:clear-window charms:*standard-window* :force-repaint t)
	       ;; move the cursor to the top left
	       (charms:move-cursor charms:*standard-window* 0 0)
	       ;; and write the time hh:mm:ss there
	       (multiple-value-bind (s m h) (get-decoded-time)
		 (charms:write-string-at-cursor
		  charms:*standard-window*
		  (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))
	       (charms:refresh-window charms:*standard-window*))
	  until (>= (- (get-universal-time) start) 10)) ; for 10 seconds
       ;; return the dimensions of the window (h x w)
       (charms:window-dimensions charms:*standard-window*)
       ))
