(defmacro => (form result)
  "Run `form` and assert that whatever it returns is EQUALP with `result`"
  (let ((aresult (gensym "result")))
    `(let ((,aresult ,form))
      (prog1 ,aresult
        (assert (equalp ,aresult ,result))))))

(defmacro *> (form output)
  "Run `form` and assert that whatever output it generates is EQUAL with `result`"
  (let ((aresult (gensym "result"))
        (aoutput (gensym "output")))
    `(let (,aresult)
      (let ((,aoutput (with-output-to-string (*standard-output*)
                        (setf ,aresult ,form))))
        (prog1 ,aresult
          (assert (equal ,aoutput ,output)))))))

(defmacro !> (form error)
  "Run `form` and assert that the error it signals is EQUAL with `error`"
  `(handler-case ,form
    (,error (condition) (format nil "~a" condition))))

#+swank (defun update-swank ()
          "Called from within the main loop, this keep the lisp REPL working
  while games run"
          (continuable
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
