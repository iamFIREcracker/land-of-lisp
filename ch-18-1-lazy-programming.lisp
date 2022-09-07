(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body)
                 ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))


#+#:excluded (defun add (a b)
               (princ "I am adding now")
               (+ a b))
#+#:excluded (defparameter *foo* (lazy (add 1 2)))
#+#:excluded (*> (force *foo*)
                 "I am adding now")


(defmacro lazy-cons (a b)
  `(lazy (cons ,a, b)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

#+#:excluded (defparameter *foo* (lazy-cons 4 7))
#+#:excluded (=> (lazy-car *foo*)
                 4)
#+#:excluded (=> (lazy-cdr *foo*)
                 '7)

(defparameter *integers*
  (labels ((f (n)
             (lazy-cons n (f (1+ n)))))
    (f 1)))
(=> (lazy-car *integers*)
    1)
(=> (lazy-car (lazy-cdr *integers*))
    2)
(=> (lazy-car (lazy-cdr (lazy-cdr *integers*)))
    3)


(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (null (force x)))


(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))


(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(=> (take 10 *integers*)
    '(1 2 3 4 5 6 7 8 9 10))

(=> (take 10 (make-lazy '(q w e r t y u i o p a s d f)))
    '(q w e r t y u i o p))


(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(=> (take-all (make-lazy '(q w e r t y u i o p a s d f)))
    '(q w e r t y u i o p a s d f))


(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(=> (take 10 (lazy-mapcar #'1+ *integers*))
    '(2 3 4 5 6 7 8 9 10 11))


(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
               (force (lazy-mapcan fun (lazy-cdr lst)))
               (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

(=> (take 10 (lazy-mapcan (lambda (x)
                            (if (evenp x)
                              (make-lazy (list x))
                              (lazy-nil)))
                          *integers*))
    '(2 4 6 8 10 12 14 16 18 20))


(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
        x
        (lazy-find-if fun (lazy-cdr lst))))))

(=> (lazy-find-if (lambda (x)
                    (eql x 1337))
                  *integers*)
    1337)


(defun lazy-nth (n lst)
  (if (zerop n)
    (lazy-car lst)
    (lazy-nth (1- n) (lazy-cdr lst))))

(=> (lazy-nth 1337 *integers*)
    1338)
