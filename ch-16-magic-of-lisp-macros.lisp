(load "utils.lisp")

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(=> (macroexpand '(let1 foo (+ 2 3)
                   (* foo foo)))
    '(let ((foo (+ 2 3)))
      (* foo foo)))


;; Repeated execution
(defmacro bogus-split (val yes no)
  `(if ,val
     (let ((head (car ,val))
           (tail (cdr ,val)))
       ,yes)
     ,no))

(*> (bogus-split '(2 3)
      (format t "This can be split into ~a and ~a." head tail)
      (format t "This cannot be split."))
    "This can be split into 2 and (3).")
(*> (bogus-split '()
      (format t "This can be split into ~a and ~a." head tail)
      (format t "This cannot be split."))
    "This cannot be split.")
(*> (bogus-split (progn (princ "Lisp rocks!")
                        '(2 3))
      (format t "This can be split into ~a and ~a." head tail)
      (format t "This cannot be split."))
    "Lisp rocks!Lisp rocks!Lisp rocks!This can be split into 2 and (3).")
(=> (macroexpand '(bogus-split (progn (princ "Lisp rocks!")
                                      '(2 3))
                   (format t "This can be split into ~a and ~a." head tail)
                   (format t "This cannot be split.")))
    '(if (progn (princ "Lisp rocks!") '(2 3))
      (let ((head (car (progn (princ "Lisp rocks!") '(2 3))))
            (tail (cdr (progn (princ "Lisp rocks!") '(2 3)))))
        (format t "This can be split into ~a and ~a." head tail))
      (format t "This cannot be split.")))


;; Variable capture
(defmacro bogus-split-1 (val yes no)
  `(let1 x ,val
     (if x
       (let ((head (car x))
             (tail (cdr x)))
         ,yes)
       ,no)))

(*> (bogus-split-1 (progn (princ "Lisp rocks!")
                          '(2 3))
      (format t "This can be split into ~a and ~a." head tail)
      (format t "This cannot be split."))
    "Lisp rocks!This can be split into 2 and (3).")
(!> (let1 x 1000
      (bogus-split-1 '(2 2)
        (+ x head)
        nil))
    simple-type-error)


(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
         (let ((head (car ,g))
               (tail (cdr ,g)))
           ,yes)
         ,no))))

#+#:excluded (macroexpand '(split '(2 3)
                            (+ x head)
                            nil))


(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
               (if tail
                 (f (cdr tail) (cons (cons head (car tail)) acc))
                 (reverse acc))
               (reverse acc))))
    (f lst nil)))

(=> (pairs '(a b c d e f))
    '((a . b) (c . d) (e . f)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))

(*> (recurse (n 9)
      (fresh-line)
      (if (zerop n)
        (princ "lift-off!")
        (progn (princ n)
               (self (1- n)))))
    "9
8
7
6
5
4
3
2
1
lift-off!")
