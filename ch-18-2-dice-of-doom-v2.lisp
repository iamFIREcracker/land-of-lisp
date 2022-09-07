(load "ch-15-dice-of-doom.lisp")
(load "ch-18-1-lazy-programming.lisp")

(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))


(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (lazy-cons (list nil
                     (game-tree (add-new-dice board player (1- spare-dice))
                                (mod (1+ player) *num-players*)
                                0
                                t))
               moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos) (first (aref board pos)))
           (dice (pos) (second (aref board pos))))
    (lazy-mapcan (lambda (src)
                   (if (eq (player src) cur-player)
                     (lazy-mapcan (lambda (dst)
                                    (if (and (not (eq (player dst) cur-player))
                                             (> (dice src) (dice dst)))
                                      (make-lazy
                                        (list
                                          (list (list src dst)
                                                (game-tree (board-attack board cur-player src dst (dice src))
                                                           cur-player
                                                           (+ spare-dice (dice dst))
                                                           nil))))
                                      (lazy-nil)))
                                  (make-lazy (neighbors src)))
                     (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum* collect n)))))


(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (third tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (first move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                     (format t "~a -> ~a" (first action) (second action))
                     (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (second  (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (third tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (second tree))))

#+#:excluded (play-vs-human (game-tree (gen-board) 0 0 t))


;; This `limit-tree-depth` function uses a pretty crude method for trimming our
;; tree: it simply trims all tree branches beyond a certain depth.  For most
;; board games, doing this is an optimal way of trimming the game tree.
;; However, Dice of Doom has the uncommon property that multiple moves in a row
;; are allowed for each player.  It would probably be more optima if
;; `limit-tree-depth` took intoccount how many times we've switched players as
;; a criterion for trimming a branch.  But our simpler version works well
;; enough.
(defun limit-tree-depth (tree depth)
  (list (first tree)
        (second tree)
        (if (zerop depth)
          (lazy-nil)
          (lazy-mapcar (lambda (move)
                         (list (first move)
                               (limit-tree-depth (second move) (1- depth))))
                       (third tree)))))

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (first tree))))
    (second (lazy-nth (position (apply #'max ratings) ratings) (third tree)))))


(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (second move) player))
                         (third tree))))

(defun rate-position (tree player)
  (let ((moves (third tree)))
    (if (not (lazy-null moves))
      (apply (if (eq (first tree) player)
               #'max
               #'min)
             (get-ratings tree player))
      (let ((w (winners (second tree))))
        (if (member player w)
          (/ 1 (length w))
          0)))))


(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (third tree)) (announce-winner (second tree)))
        ((zerop (first tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

#+#:excluded (play-vs-computer (game-tree (gen-board) 0 0 t))

(defun computer-vs-computer (tree)
  (print-info tree)
  (if (lazy-null (third tree))
    (announce-winner (second tree))
    (computer-vs-computer (handle-computer tree))))

#+#:excluded (computer-vs-computer (game-tree (gen-board) 0 0 t))


;; In version 1 of our Dice of Doom code, the AI player had no reason to ever
;; worrybout its margin of victory.  All it cared about was that when the game
;; ended, it had ownership of at least one more territory of the board than its
;; opponent, which meant it had won.
;;
;; However, now that we're using imprecise heuristics in our AI code, it
;; matters _a lot_ how large the lead is at any point in the game.  A heuristic
;; rule for this situation is "If I am totally whomping my opponent in the
;; game, it is pretty unlikely he/she will be able to recover, even if I look
;; only a few moves ahead."
;;
;; Remember that a minimax algorithm assigns a point score to every final leaf
;; branch in thre tree.  In version 1 of our game, this score was either 0 or
;; 1, or sometimes 1/2 when the game ended in a tie.  In version 2, these are
;; not truly "final leaves" in the tree, but simply leaves in our much smaller
;; trimmed tree.  In this situation, it would be much better if our leaf scores
;; had a larger range of values, so that we can tell which moves lead to a game
;; we're winning by "a lot" and which moves lead to a game we're winning by
;; only "a little".
(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (first hex) player)
              (if (threatened pos board)
                1
                2)
              -1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (first hex))
         (dice (second hex)))
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n))
                    (nplayer (first nhex))
                    (ndice (second nhex)))
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return t))))))

(defun rate-position (tree player)
  (let ((moves (third tree)))
    (if (not (lazy-null moves))
      (apply (if (eq (first tree) player)
               #'max
               #'min)
             (get-ratings tree player))
      (score-board (second tree) player))))

#+#:excluded (computer-vs-computer (game-tree (gen-board) 0 0 t))


;; Alpha-beta pruning is a well-known optimization of the minimaxz algorithm
;; that improves performance by skipping over some branches (_pruning_ those
;; branches) if it is certain that they will not impact the final minimax
;; evaluation.
;;
;; The `get-ratings` function was responsible for calculating the best score
;; out of multiple available moves from a single-board arrangement.  Now,
;; however, we want it to stop early in its evaluation of moves once it decides
;; it has found a move that's "as good as is possible."  Determining whether it
;; has reached this point is subtly different dependeing on whether the node in
;; question is a MAX node (a move of the current player) or a MIN move (a move
;; for the opponent).

;; Version of `get-ratings` responsible for MAX nodes first.
;;
;; When rating the next branch of the tree (by calling `ab-rate-position`,
;; which we'll write shortly), we save the result as `x`.  If `x` is greater
;; than or equal to our `upper-limit`, we know we got a result as good as we
;; can hope for, andcan just return the latest rating as a final value in our
;; list.  If `x` isn't large enough, we need to keep looking at the remaining
;; branches.
(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (second (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (>= x upper-limit)
                   (list x)
                   (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (third tree) lower-limit)))

;; Version of `get-ratings` responsible for MIN nodes.
(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (second (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (<= x lower-limit)
                   (list x)
                   (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (third tree) upper-limit)))


(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (third tree)))
    (if (not (lazy-null moves))
      (if (eq (first tree) player)
        (apply #'max (ab-get-ratings-max tree
                                         player 
                                         upper-limit 
                                         lower-limit))
        (apply #'min (ab-get-ratings-min tree
                                         player 
                                         upper-limit 
                                         lower-limit)))
      (score-board (second tree) player))))


(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*) 
                                     (first tree)
                                     most-positive-fixnum
                                     most-negative-fixnum)))
    (second (lazy-nth (position (apply #'max ratings) ratings) (third tree)))))


(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))


#+#:excluded (play-vs-computer (game-tree (gen-board) 0 0 t))
#+#:excluded (computer-vs-computer (game-tree (gen-board) 0 0 t))
