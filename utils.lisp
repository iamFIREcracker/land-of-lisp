(defun try-update-swank ()
  "Called from within the main loop, this keep the lisp REPL working
  while games run"
  #+swank (continuable
    (let ((conn (or swank::*emacs-connection* (swank::default-connection))))
      (when conn
        (swank::handle-requests conn t)))))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
  error. Remember to hit C in slime or pick the restart so
  errors don't kill the app."
  `(restart-case
    (progn ,@body)
    (continue () :report "Continue")))
