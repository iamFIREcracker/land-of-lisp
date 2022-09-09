(load "utils.lisp")
(load "ch-17-1-domain-specific-languages.lisp")


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


#+#:excluded (defun random-walk (value length)
               (unless (zerop length)
                 (cons value
                       (random-walk (if (zerop (random 2))
                                      (1- value)
                                      (1+ value))
                                    (1- length)))))

#+#:excluded (random-walk 100 10)
; (100 101 102 103 104 105 106 107 108 109)


#+#:excluded (with-open-file (*standard-output*
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
