(defparameter *wizard-nodes*  '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
                                (garden (you are in the beautiful garden. there is a well in front of you.))
                                (attic (you are in the attic. there is a giant welding torch in the corner.))))
(defparameter *wizard-edges*  '((living-room (garden west door)
                                             (attic upstairs ladder))
                                (garden (living-room east door))
                                (attic (living-room downstairs ladder))))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

#+#:excluded (dot-name 'living-room)
#+#:excluded (dot-name 'foo!)
#+#:excluded (dot-name '24)

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

#+#:excluded (dot-label '(living-room (you are in the living-room.)))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes)
  (values))

#+#:excluded (nodes->dot *wizard-nodes*)

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges)
  (values))

#+#:excluded (edges->dot *wizard-edges*)

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}")
  (values))

#+#:excluded (graph->dot *wizard-nodes* *wizard-edges*)

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (uiop:run-program `("dot" "-Tpng" "-O" ,fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

#+#:excluded (graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}")
  (values))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

#+#:excluded (ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges*)
