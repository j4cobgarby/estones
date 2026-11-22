;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst estones-columns "ABCDEFGHJKLMNOPQRSTUVWXYZ"
  "Column letters for board coords (no I)")

(cl-defstruct (estones-move (:constructor estones-move--make))
  "Represents one go move.
COLOUR is the player's stone colour, `black' or `white';
IDX is the index in the board grid of the stone's position."
  colour                                
  idx)

(cl-defstruct (estones-board (:constructor estones-board--make) (:copier nil))
  "State of a go board of SIZE rows (and SIZE columns), and a given KOMI value.
GRID is a vector representing the board matrix;
KO is the active ko point, or nil if there isn't one;
PRISONERS is a tuple (black's prisoners . white's prisoners)."
  size
  komi
  grid
  ko
  prisoners)

(defun estones-board-deep-copy (board)
  (estones-board--make :size (estones-board-size board)
                   :komi (estones-board-komi board)
                   :grid (copy-sequence (estones-board-grid board))
                   :ko (estones-board-ko board)
                   :prisoners (let ((prisoners (estones-board-prisoners board)))
                                (cons (car prisoners) (cdr prisoners)))))

(cl-defstruct (estones-node (:constructor estones-node--make))
  "Builds up a tree representing a whole go game.
Each node in the tree stores a copy of the board BOARD-SNAPSHOT after that move,
as well as the MOVE that led to it."
  move                                  ; an estones-move
  board-snapshot                        ; a copy of an estones-board
  parent                                ; a reference to the previous estones-node in the tree
  children                              ; all continuations
  main-move
  move-number)

(cl-defstruct (estones-game (:constructor estones-game--make))
  "A container for a whole game (an `estones-node') + metadata."
  root                                  ; Root `estones-node'
  main-leaf                             ; Current frontier of mainline game
  black-name                            ; Black player's name
  white-name                            ; White player's name
  black-is-local                        ; t if I can make moves for black
  white-is-local                        ; t if I can make moves for white
  (has-ended nil)                       ; t if game has ended
  title)                                ; Name of the game

(defun estones-local-to-playp (game)
  "Is it currently a local player's turn in GAME, following the mainline leaf node?"
  (if (eql 'black (estones-next-move-colour (estones-node-move (estones-game-main-leaf game))))
      (estones-game-black-is-local game)
    (estones-game-white-is-local game)))

(defun estones-setup-board (board size)
  "Initialises a board by setting its SIZE. Its grid is set to a blank grid of the correct size."
  (unless (and (integerp size) (<= 2 size (length estones-columns)))
    (user-error "Bad board size: %s" size))
  (setf (estones-board-size board) size)
  (let ((size (estones-board-size board)))
    (setf (estones-board-grid board) (make-vector (* size size) nil)
          (estones-board-ko board) nil
          (estones-board-prisoners board) (cons 0 0)))
  board)

(defun estones-idx (board x y)
  "Convert x-y coords to vector index"
  (+ x (* y (estones-board-size board))))

(defun estones-xy (board idx)
  "Convert vector index to x-y coords"
  (let ((size (estones-board-size board)))
    (cons (% idx size) (/ idx size))))

(defun estones-coords (board idx)
  (pcase-let* ((`(,x . ,y) (estones-xy board idx))
               (column (aref estones-columns x))
               (row (number-to-string
                     (- (estones-board-size board) y))))
    (format "%c%s" column row)))

(defun estones-stone-at (board x y)
  "Return stone at coords X, Y"
  (aref (estones-board-grid board) (estones-idx board x y)))

(defun estones-board-neighbours (board idx)
  "Adjacent neighbours of point IDX."
  (let* ((size (estones-board-size board))
         (xy (estones-xy board idx))
         (x (car xy))
         (y (cdr xy))
         neighbs)
    (when (> x 0) (push (1- idx) neighbs))
    (when (< x (1- size)) (push (1+ idx) neighbs))
    (when (> y 0) (push (- idx size) neighbs))
    (when (< y (1- size)) (push (+ idx size) neighbs))
    neighbs))

(defun estones-other-colour (col)
  (pcase col
    ('black 'white)
    (_ 'black)))

(defun estones-next-move-colour (prev-move)
  (if prev-move
      (estones-other-colour (estones-move-colour prev-move))
    'black))
    
(defun estones-group (board idx stones liberties)
  "Find the extent of a group including stone at IDX.
STONES and LIBERTIES are used for recursion, so just pass them as nil when calling this.
Returns (STONES . LIBERTIES), where STONES is a list of all stones in the group, and LIBERTIES is all adjacent empty points."
  (let* ((grid (estones-board-grid board))
         (colour (aref grid idx)))
    (unless (member idx stones)
      (push idx stones))
    (dolist (neighb-idx (estones-board-neighbours board idx))
      (unless (or
               (member neighb-idx stones)
               (member neighb-idx liberties))
        (let ((neighb (aref grid neighb-idx)))
          (if neighb
              (when (eq neighb colour)
                (let ((rec-pair (estones-group board neighb-idx (cons neighb-idx stones) liberties)))
                  (setq stones (car rec-pair))
                  (setq liberties (cdr rec-pair))))
            (unless (member neighb-idx liberties)
              (push neighb-idx liberties))))))
    (cons stones liberties)))

(defun estones-play (board colour idx)
  "Play a COLOUR stone on the BOARD at index IDX.
If IDX is nil, pass.
Returns the new move, and also modifies the BOARD's history mainline."
  (let* ((grid (estones-board-grid board))
         (size (estones-board-size board))
         (prev-ko (estones-board-ko board))
         (opp (estones-other-colour colour))
         (captured-stones nil)
         (new-ko nil))
    (when idx                           ; Playing a stone (otherwise pass)
      ;; Initial checks to see if the stone can be legal
      (when (or (< idx 0) (>= idx (length grid)))
        (user-error "Index out of bounds: %s" idx))
      (when (aref grid idx)
        (user-error "Illegal move: %s point already occupied" idx))
      (when (eql idx prev-ko)
        (user-error "Illegal move: %s recaptures ko point" idx))
      (aset grid idx colour)
      ;; Check if the new stone captures any neighbouring groups
      (dolist (neighb (estones-board-neighbours board idx))
        (when (eql (aref grid neighb) opp)
          (let ((group (estones-group board neighb nil nil)))
            (unless (cdr group)
              (dolist (taken (car group))
                (aset grid taken nil)
                (push taken captured-stones))))))
      (let ((group (estones-group board idx nil nil)))
        ;; Check if new stone is suicicide
        (unless (cdr group)
          (aset grid idx nil)
          (user-error "Illegal move: %s is suicide!" idx))
        ;; Check for new ko (if the new stone is on its own, capturing another, and has
        ;; now only one liberty)
        (when (and (= (length captured-stones) 1)
                   (= (length (car group)) 1)
                   (= (length (cdr group)) 1))
          (setq new-ko (car captured-stones))))
      (if (eq colour 'black)
          (incf (car (estones-board-prisoners board)) (length captured-stones))
        (incf (cdr (estones-board-prisoners board)) (length captured-stones))))
    (setf (estones-board-ko board) new-ko)
    (estones-move--make :colour colour :idx idx)))

(defun estones-new-gametree (size komi)
  "Creates a brand new game with given board SIZE and KOMI, returning the root node."
  (estones-node--make :board-snapshot (let ((board (estones-board--make :komi komi)))
                                    (estones-setup-board board size))
                      :move-number 0))

(defun estones-new-local-game (size komi &optional black-name white-name title)
  "Creates a new game with given board SIZE and KOMI."
  (estones-game--make
   :root (estones-new-gametree size komi)
   :black-name (or black-name "Black")
   :white-name (or white-name "White")
   :title (or title "Unnamed Local Game")
   :black-is-local t
   :white-is-local t))

(defun estones-play-after-node (node colour idx)
  "Play a new move in a game tree after a given NODE, also setting it as the main continuation.
If COLOUR is nil, play stones taking in turns between colours.
If a move at IDX already has been played after NODE, it's not added again, and the exist continuation is returned.
Otherwise, the new node is returned."
  
  (let ((existing (any (lambda (move)
                         (= (estones-move-idx move) idx)) (estones-node-children node))))
    (if existing
        (car existing)
      (unless colour
        (setq colour (estones-next-move-colour (estones-node-move node))))
      (let* ((board-cpy (estones-board-deep-copy (estones-node-board-snapshot node)))
             (new-move (estones-play board-cpy colour idx))
             (new-node (estones-node--make :move new-move
                                           :board-snapshot board-cpy
                                           :parent node
                                           :children nil
                                           :main-move nil
                                           :move-number (1+ (estones-node-move-number node)))))
        (push new-node (estones-node-children node))
        (setf (estones-node-main-move node) new-node)))))

(provide 'estones-functionality)
