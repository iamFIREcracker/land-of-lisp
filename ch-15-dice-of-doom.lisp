(load "utils.lisp")

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))


(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

#+#:excluded (gen-board)


(defun player-letter (n)
  (code-char (+ 97 n)))

(=> (player-letter 1)
    #\b)
(=> (player-letter 10)
    #\k)


(defun draw-board (board)
  (loop for y below *board-size* 
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y) do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                   (second hex))))))

(*> (draw-board #((0 3) (0 3) (1 3) (1 1)))
    "    a-3 a-3 
  b-3 b-1 ")


(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))


(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (cons (list nil
                (game-tree (add-new-dice board player (1- spare-dice))
                           (mod (1+ player) *num-players*)
                           0
                           t))
          moves)))


(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos) (first (aref board pos)))
           (dice (pos) (second (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                              (list (list src dst)
                                    (game-tree (board-attack board cur-player src dst (dice src))
                                               cur-player
                                               (+ spare-dice (dice dst))
                                               nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum* collect n))))


(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (labels ((firstp (pos) (zerop (mod pos *board-size*)))
             (lastp (pos) (firstp (1+ pos))))
      (loop for p in (append (list up down)
                             (unless (firstp pos)
                               (list (1- up) (1- pos)))
                             (unless (lastp pos)
                               (list (1+ down) (1+ pos))))
            when (and (>= p 0) (< p *board-hexnum*))
            collect p))))

(=> (neighbors 0)
    '(2 3 1))
(=> (neighbors 3)
    '(1 0 2))


(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

(=> (board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3)
    #((0 3) (0 1) (1 3) (0 2)))


(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   (t (let ((cur-player (first (car lst)))
                            (cur-dice (second (car lst))))
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                          (cons (list cur-player (1+ cur-dice))
                                (f (cdr lst) (1- n)))
                          (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

(=> (add-new-dice #((0 1) (1 3) (0 2) (1 1)) 0 2)
    #((0 2) (1 3) (0 3) (1 1)))


(=> (game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t)
    ; The game tree first lists the current player number, the
    ; layout of the board, and then the legal moves for that
    ; context.  For the initial board position, at the beginning
    ; of player A's turn, there is only possible move: the player
    ; can move from hexagon 2 to hexagon 3, capturing player B's
    ; die in that spot.  After that, the player can pass.  Player
    ; B now has no move available.  Sincethis player's game tree
    ; has no available moves listed, the game has ended, with
    ; a win for player A.
    '(0
      #((0 1) (1 1) (0 2) (1 1))
      (((2 3) (0
               #((0 1) (1 1) (0 1) (0 1))
               ((nil (1
                      #((0 1) (1 1) (0 1) (0 1))
                      nil))))))))


(defun play-vs-human (tree)
  (print-info tree)
  (if (third tree)
    (play-vs-human (handle-human tree))
    (announce-winner (second tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (second tree)))

(*> (print-info (game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t))
    "current player = a
    a-1 b-1 
  a-2 b-1 ")


(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (third tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (first move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                 (format t "~a -> ~a" (first action) (second action))
                 (princ "end turn"))))
    (fresh-line)
    (second  (nth (1- (read)) moves))))

(*> (with-input-from-string (*standard-input* "1")
      (handle-human (game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t)))
    "choose your move:
1. 2 -> 3
")


(defun winners (board)
  (let* ((tally (loop for hex across board
                      collect (first hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(=> (winners #((0 1) (1 1) (0 2) (1 1)))
    '(0 1))
(=> (winners #((0 1) (0 1) (0 2) (1 1)))
    '(0))


(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game is a tie between ~a" (mapcar #'player-letter w))
      (format t "The winner is ~a" (player-letter (first w))))))

#+#:excluded (play-vs-human (game-tree #((1 2) (0 3) (1 1) (1 1)) 0 0 t))
; current player = a
;     b-2 a-3 
;   b-1 b-1 
; choose your move:
; 1. 1 -> 3
; 2. 1 -> 0
; 2
; current player = a
;     a-2 a-1 
;   b-1 b-1 
; choose your move:
; 1. end turn
; 2. 0 -> 2
; 3. 0 -> 3
; 2
; current player = a
;     a-1 a-1 
;   a-1 b-1 
; choose your move:
; 1. end turn
; 1
; current player = b
;     a-2 a-2 
;   a-1 b-1 
; The winneer is a


; Turns out that for a two-player board game, a simple method exists to model
; what an opponent will do.  We simply accept the truism "What is good for my
; opponent is bad for me."  This means we can use the following approach to
; model a move for the opponent:
;
; 1. Look at each available move
; 2. Give a point rating to the board position resulting from each move
; 3. Pick the move with the minimum point rating
;
; This algorithm for estimating what an oponent will do is identical to the
; one used for the primary player, except that in step 3, we pick the move
; with the _minimum_ instead of _maximum_ rating.  The benefit of this
; approach, called the _minimax algorithm_, is that we use the same point
; ratings when working out the opponent's moves that we use for the primary AI
; player, but then just tweak the third step a little to adjust. 
(defun rate-position (tree player)
  (let ((moves (third tree)))
    (if moves
      (apply (if (eq (first tree) player)
               #'max
               #'min)
             (get-ratings tree player))
      (let ((w (winners (second tree))))
        (if (member player w)
          (/ 1 (length w))
          0)))))

(defun get-ratings (tree player)
  (mapcar (lambda (move)
            (rate-position (second move) player))
          (third tree)))

(=> (get-ratings (game-tree #((1 2) (0 3) (1 1) (1 1)) 0 0 t) 0)
    '(1 1))
(=> (rate-position (game-tree #((1 2) (0 3) (1 1) (1 1)) 0 0 t) 0)
    1)

(=> (get-ratings (game-tree #((1 2) (0 3) (1 1) (1 1)) 0 0 t) 1)
    '(0 0))
(=> (rate-position (game-tree #((1 2) (0 3) (1 1) (1 1)) 0 0 t) 1)
    0)

(=> (get-ratings (game-tree #((0 2) (0 2) (1 2) (1 2)) 0 0 t) 1)
    nil)
(=> (rate-position (game-tree #((0 2) (0 2) (1 2) (1 2)) 0 0 t) 1)
    1/2)


(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (first tree))))
    (second (nth (position (apply #'max ratings) ratings) (third tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (third tree)) (announce-winner (second tree)))
        ((zerop (first tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))


#+#:excluded (play-vs-computer (game-tree #((1 1) (1 3) (1 3) (0 2)) 0 0 t))
; current player = a
;     b-1 b-3 
;   b-3 a-2 
; choose your move:
; 1. 3 -> 0
; 1
; current player = a
;     a-1 b-3 
;   b-3 a-1 
; choose your move:
; 1. end turn
; 1
; current player = b
;     a-1 b-3 
;   b-3 a-1 
; current player = b
;     a-1 b-1 
;   b-3 b-2 
; current player = a
;     a-1 b-1 
;   b-3 b-2 
; The winner is b


(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))


;; Memoization
(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test 'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))

(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab) (funcall old-rate-position tree player))))))


;; Tail-Call Optimization
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
             (cond ((zerop n) (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (first (car lst)))
                            (cur-dice (second (car lst))))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                          (f (cdr lst) (1- n) (cons (list cur-player (1+ cur-dice)) acc))
                          (f (cdr lst) n      (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice '()))))

#+#:excluded (play-vs-computer (game-tree (gen-board) 0 0 t))


(defun computer-vs-computer (tree)
  (print-info tree)
  (if (null (third tree))
    (announce-winner (second tree))
    (computer-vs-computer (handle-computer tree))))

#+#:excluded (computer-vs-computer (game-tree (gen-board) 0 0 t))
