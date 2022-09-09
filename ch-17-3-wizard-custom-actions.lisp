(load "utils.lisp")
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
