(load "utils.lisp")
(load "ch-15-magic-of-lisp-macros.lisp")


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


;; SVG
(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ,@body))


(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max (+ x amt) 0)))
          col))

(=> (brightness '(255 0 0) -100)
    '(155 0 0))


(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color (brightness color -100))))

(=> (svg-style '(255 0 0))
    "fill:rgb(255,0,0);stroke:rgb(155,0,0)")


(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

(*> (svg (circle '(50 . 50) 50 '(255 0 0))
         (circle '(100 . 100) 50 '(0 0 255)))
    "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><circle cx=\"50\" cy=\"50\" r=\"50\" style=\"fill:rgb(255,0,0);stroke:rgb(155,0,0)\"></circle><circle cx=\"100\" cy=\"100\" r=\"50\" style=\"fill:rgb(0,0,255);stroke:rgb(0,0,155)\"></circle></svg>")


(defun polygon (points color)
  (tag polygon (points (format nil "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))

(*> (polygon (loop for n below 10 collect (cons n n)) '(255 0 0))
    "<polygon points=\"0,0 1,1 2,2 3,3 4,4 5,5 6,6 7,7 8,8 9,9 \" style=\"fill:rgb(255,0,0);stroke:rgb(155,0,0)\"></polygon>")


(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                         (1- value)
                         (1+ value))
                       (1- length)))))

#+#:excluded (random-walk 100 10)
; (100 101 102 103 104 105 106 107 108 109)


(with-open-file (*standard-output*
                  "random_walk.svg"
                  :direction :output
                  :if-exists :supersede)
  (svg (loop repeat 10
             do (polygon (append '((0 . 200))
                                 (loop for x from 0
                                       for y in (random-walk 100 400)
                                       collect (cons x y))
                                 '((400 . 200)))
                         (loop repeat 3
                               collect (random 256))))))


;; Custom Game commands for Wizard's Adventure Game
(load "ch-06-setting-up-custom-repl.lisp")

#+#:excluded (game-repl)

(defun have (object)
  (member object (inventory)))


(defparameter *chain-welded* nil)

(defun weld (subject object)
  (if (and (eq *location* 'attic)
           (eq subject 'chain)
           (eq object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
    (progn (setf *chain-welded* t)
           '(the chain is now securely werlded to the bucket.))
    '(you cannot weld like that.)))

(=> (weld 'chain 'bucket)
    '(you cannot weld like that.))

#+#:excluded (game-repl)
; weld chain bucket
; I do not know that command.
; quit

(pushnew 'weld *allowed-commands*)

#+#:excluded (game-repl)
; weld chain bucket
; You cannot weld like that.
; quit


(defparameter *bucket-filled* t)

(defun dunk (subject object)
  (if (and (eq *location* 'garden)
           (eq subject 'bucket)
           (eq object 'well)
           (have 'bucket)
           *chain-welded*)
    (progn (setf *bucket-filled* t)
           '(the bucket is now full of water))
    '(you cannot dunk like that.)))

(pushnew 'dunk *allowed-commands*)

#+#:excluded (game-repl)
; dunk bucket well
; You cannot dunk like that.
; quit


(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
              ,@body
              '(i cant ,command like that.)))
     (pushnew ',command *allowed-commands*)))


(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
    (progn (setf *chain-welded* t)
           '(the chain is now securely welded to the bucket.))
    '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welded*
    (progn (setf *bucket-filled* t)
           '(the bucket is now full of water))
    '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
        ((have 'frog) '(the wizard awakens and sees that you stole his frog. he
                        is so upset he banishes you to the netherworlds- you
                        lose! the end.))
        (t '(the wizard awakes from his slumber and greets you warmly.
             he hands you the magic low-carb donut- you win! the end.))))

#+#:excluded (game-repl)
; look
; You are in the living-room. A wizard is snoring loudly on the couch. There is
;  a door going west from here. There is a ladder going upstairs from here. You
;  see a whiskey on the floor. You see a bucket on the floor.
; pickup bucket
; You are now carrying the bucket
; pickup whiskey
; You are now carrying the whiskey
; inventory
; Items- whiskey bucket
; walk upstairs
; You are in the attic. There is a giant welding torch in the corner. There is a
;  ladder going downstairs from here.
; walk east
; You cannot go that way.
; walk downstairs
; You are in the living-room. A wizard is snoring loudly on the couch. There is
;  a door going west from here. There is a ladder going upstairs from here.
; walk west
; You are in the beautiful garden. There is a well in front of you. There is a
;  door going east from here. You see a frog on the floor. You see a chain on the
;  floor.
; dunk bucket well
; The water level is too low to reach.
; pickup chain
; You are now carrying the chain
; walk east
; You are in the living-room. A wizard is snoring loudly on the couch. There is
;  a door going west from here. There is a ladder going upstairs from here.
; splash bucket wizard
; The bucket has nothing in it.
; quit
