(load "utils.lisp")
(load "ch-16-magic-of-lisp-macros.lisp")


(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))

(*> (print-tag 'mytag '((color . blue) (height . 9)) nil)
    "<mytag color=\"BLUE\" height=\"9\">")


(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
     ,@body
     (print-tag ',name nil t)))

(=> (macroexpand '(tag mytag (color 'blue height (+ 4 5))))
    '(progn (print-tag 'mytag
                       (list (cons 'color 'blue)
                             (cons 'height (+ 4 5)))
                       nil)
      (print-tag 'mytag nil t)))
(*> (tag mytag (color 'blue height (+ 4 5)))
    "<mytag color=\"BLUE\" height=\"9\"></mytag>")
(*> (tag mygatg (color 'blue size 'big)
      (tag first-inner-tag ())
      (tag second-inner-tag ()))
    "<mygatg color=\"BLUE\" size=\"BIG\"><first-inner-tag></first-inner-tag><second-inner-tag></second-inner-tag></mygatg>")
(*> (tag html ()
      (tag body ()
        (princ "Hello World!")))
    "<html><body>Hello World!</body></html>")


;; HTML
(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

(*> (html (body (princ "Hello World!")))
    "<html><body>Hello World!</body></html>")
