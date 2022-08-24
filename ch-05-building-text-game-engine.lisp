(defparameter *nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
                        (garden (you are in the beautiful garden. there is a well in front of you.))
                        (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

#+#:excluded (describe-location 'living-room *nodes*)
#+#:excluded (assoc 'living-room *nodes*)

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

#+#:excluded (describe-path '(garden west door))
#+#:excluded (describe-path '(attic upstairs ladder))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

#+#:excluded (describe-paths 'living-room *edges*)

(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

#+#:excluded (objects-at 'living-room *objects* *object-locations*)

(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

#+#:excluded (describe-objects 'living-room *objects* *object-locations*)

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

#+#:excluded (look)

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

#+#:excluded (look)
#+#:excluded (walk 'west)
#+#:excluded (walk 'east)

(defmacro mwalk (direction)
  `(walk ',direction))

#+#:excluded (mwalk west)
#+#:excluded (mwalk east)

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t `(you cannot get that.))))

#+#:excluded (look)
#+#:excluded (pickup 'bucket)
#+#:excluded (objects-at 'body *objects* *object-locations*)
#+#:excluded (objects-at 'living-room *objects* *object-locations*)

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

#+#:excluded (inventory)
