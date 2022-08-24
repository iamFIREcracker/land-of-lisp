(load "ch-05-building-text-game-engine.lisp")

(defun game-repl ()
  (loop (print (eval (read)))))

#+#:excluded (game-repl)

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

#+#:excluded (game-read)

(defparameter *allowed-commandb* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

#+#:excluded (game-eval '(inventory))

(defun tweak-text (lst caps lit)
  "When `caps` is T, the next char will be capitalized. ~
  When `lit` is T, the next char will be printed as is ~
  (it will be down-cased otherwise)."
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\Space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

#+#:excluded (game-print '(not only does this sentence have a "comma," it also mentions the "iPad."))

#+#:excluded (game-repl)
