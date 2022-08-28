(load "utils.lisp")

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\Space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16
                :junk-allowed t)))
    (if code
      (code-char code)
      default)))

#+#:excluded (=> (decode-param "foo")
                 "foo")
#+#:excluded (=> (decode-param "foo%3F")
                 "foo?")
#+#:excluded (=> (decode-param "foo+bar")
                 "foo bar")


(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

#+#:excluded (=> (parse-params "name=bob&age=25&gender=male")
                 '((name . "bob") (age . "25") (gender . "male")))


(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\Space s))
                      (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
      (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
      (cons url '()))))

#+#:excluded (=> (parse-url "GET /lolcats.html HTTP/1.1")
                 '("lolcats.html"))

#+#:excluded (=> (parse-url "GET /lolcats.html?extra-funny=yes HTTP/1.1")
                 '("lolcats.html" (extra-funny . "yes")))


(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

#+#:excluded (=> (get-header (make-string-input-stream "foo: 1
bar: abc, 123

"))
                 '((foo . "1") (bar . "abc, 123")))


(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

#+#:excluded (=> (get-content-params (make-string-input-stream "") nil)
                 nil)
#+#:excluded (=> (get-content-params (make-string-input-stream "foo=bar&baz=qux") '((content-length . "15")))
                 '((foo . "bar") (baz . "qux")))


(ql:quickload "trivial-sockets")

(defun serve (request-handler)
  (let ((server (trivial-sockets:open-server :port 8080 :reuse-address t)))
    (unwind-protect
      (loop (with-open-stream (stream (trivial-sockets:accept-connection server))
              (let* ((url (parse-url (read-line stream)))
                     (path (car url))
                     (header (get-header stream))
                     (params (append (cdr url)
                                     (get-content-params stream header)))
                     (*standard-output* stream))
                (funcall request-handler path header params))))
      (trivial-sockets:close-server server))))


(defun hello-request-handler (path header params)
  (declare (ignore header))
  (if (equal path "greeting")
    (let ((name (assoc 'name params)))
      (if (not name)
        (princ "<html><form>What is yourname?<input name='name'/></form></html>")
        (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
    (princ "Sorry... I don't know that page.")))

#+#:excluded (*> (hello-request-handler "lolcats" '() '())
                 "Sorry... I don't know that page.")
#+#:excluded (*> (hello-request-handler "greeting" '() '())
                 "<html><form>What is yourname?<input name='name'/></form></html>")
#+#:excluded (*> (hello-request-handler "greeting" '() '((name . "Bob")))
                 "<html>Nice to meet you, Bob!</html>")

#+#:excluded (serve 'hello-request-handler)
#|
Modern browsers do not support HTTP/0.9 anymore, so 
you will have to use `curl` to test instead

$ curl --http0.9 http://localhost:8080
Sorry... I don't know that page.
$ curl --http0.9 http://localhost:8080/greeting
<html><form>What is yourname?<input name='name'/></form></html>
$ curl --http0.9 http://localhost:8080/greeting --data "name=matteo"
<html>Nice to meet you, matteo!</html>
|#
