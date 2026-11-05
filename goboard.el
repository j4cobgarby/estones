;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst ego-columns "ABCDEFGHJKLMNOPQRSTUVWXYZ"
  "Column letters for board coords (no I)")

(cl-defstruct (ego-move (:constructor ego-move--make))
  "Represents one go move.
COLOUR is the player's stone colour, `black' or `white';
IDX is the index in the board grid of the stone's position."
  colour                                
  idx)

(cl-defstruct (ego-board (:constructor ego-board--make) (:copier nil))
  "State of a go board of SIZE rows (and SIZE columns), and a given KOMI value.
GRID is a vector representing the board matrix;
KO is the active ko point, or nil if there isn't one;
PRISONERS is a tuple (black's prisoners . white's prisoners)."
  size
  komi
  grid
  ko
  prisoners)

(defun ego-board-deep-copy (board)
  (ego-board--make :size (ego-board-size board)
                   :komi (ego-board-komi board)
                   :grid (copy-sequence (ego-board-grid board))
                   :ko (ego-board-ko board)
                   :prisoners (let ((prisoners (ego-board-prisoners board)))
                                (cons (car prisoners) (cdr prisoners)))))

(cl-defstruct (ego-node (:constructor ego-node--make))
  "Builds up a tree representing a whole go game.
Each node in the tree stores a copy of the board BOARD-SNAPSHOT after that move,
as well as the MOVE that led to it."
  move                                  ; an ego-move
  board-snapshot                        ; a copy of an ego-board
  parent                                ; a reference to the previous ego-node in the tree
  children                              ; all continuations
  main-move)                            ; the mainline continuation

(defun ego-setup-board (board size)
  "Initialises a board by setting its SIZE. Its grid is set to a blank grid of the correct size."
  (unless (and (integerp size) (<= 2 size (length ego-columns)))
    (user-error "Bad board size: %s" size))
  (setf (ego-board-size board) size)
  (let ((size (ego-board-size board)))
    (setf (ego-board-grid board) (make-vector (* size size) nil)
          (ego-board-ko board) nil
          (ego-board-prisoners board) (cons 0 0)))
  board)

(defun ego-idx (board x y)
  "Convert x-y coords to vector index"
  (+ x (* y (ego-board-size board))))

(defun ego-xy (board idx)
  "Convert vector index to x-y coords"
  (let ((size (ego-board-size board)))
    (cons (% idx size) (/ idx size))))

(defun ego-stone-at (board x y)
  "Return stone at coords X, Y"
  (aref (ego-board-grid board) (ego-idx board x y)))

(defun ego-board-neighbours (board idx)
  "Adjacent neighbours of point IDX."
  (let* ((size (ego-board-size board))
         (xy (ego-xy board idx))
         (x (car xy))
         (y (cdr xy))
         neighbs)
    (when (> x 0) (push (1- idx) neighbs))
    (when (< x (1- size)) (push (1+ idx) neighbs))
    (when (> y 0) (push (- idx size) neighbs))
    (when (< y (1- size)) (push (+ idx size) neighbs))
    neighbs))

(defun ego-other-colour (col)
  (pcase col
    ('white 'black)
    ('black 'white)
    (_ (user-error "other-colour not valid on %s" col))))
    
(defun ego-group (board idx stones liberties)
  "Find the extent of a group including stone at IDX.
STONES and LIBERTIES are used for recursion, so just pass them as nil when calling this.
Returns (STONES . LIBERTIES), where STONES is a list of all stones in the group, and LIBERTIES is all adjacent empty points."
  (let* ((grid (ego-board-grid board))
         (colour (aref grid idx)))
    (unless (member idx stones)
      (push idx stones))
    (dolist (neighb-idx (ego-board-neighbours board idx))
      (unless (or
               (member neighb-idx stones)
               (member neighb-idx liberties))
        (let ((neighb (aref grid neighb-idx)))
          (if neighb
              (when (eq neighb colour)
                (let ((rec-pair (ego-group board neighb-idx (cons neighb-idx stones) liberties)))
                  (setq stones (car rec-pair))
                  (setq liberties (cdr rec-pair))))
            (unless (member neighb-idx liberties)
              (push neighb-idx liberties))))))
    (cons stones liberties)))

(defun ego-play (board colour idx)
  "Play a COLOUR stone on the BOARD at index IDX.
If IDX is nil, pass.
Returns the new move, and also modifies the BOARD's history mainline."
  (let* ((grid (ego-board-grid board))
         (size (ego-board-size board))
         (prev-ko (ego-board-ko board))
         (opp (ego-other-colour colour))
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
      (dolist (neighb (ego-board-neighbours board idx))
        (when (eql (aref grid neighb) opp)
          (let ((group (ego-group board neighb nil nil)))
            (unless (cdr group)
              (dolist (taken (car group))
                (aset grid taken nil)
                (push taken captured-stones))))))
      (let ((group (ego-group board idx nil nil)))
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
          (incf (car (ego-board-prisoners board)) (length captured-stones))
        (incf (cdr (ego-board-prisoners board)) (length captured-stones))))
    (setf (ego-board-ko board) new-ko)
    (ego-move--make :colour colour :idx idx)))

(defun ego-new-gametree (size komi)
  "Creates a brand new game with given board SIZE and KOMI, returning the root node."
  (ego-node--make :board-snapshot (let ((board (ego-board--make :komi komi)))
                                    (ego-setup-board board size))))

(defun ego-play-after-node (node colour idx)
  "Play a new move in a game tree after a given NODE, also setting it as the main continuation."
  (let* ((board-cpy (ego-board-deep-copy (ego-node-board-snapshot node)))
         (new-move (ego-play board-cpy colour idx))
         (new-node (ego-node--make :move new-move
                                   :board-snapshot board-cpy
                                   :parent node
                                   :children nil
                                   :main-move nil)))
    (push new-node (ego-node-children node))
    (setf (ego-node-main-move node) new-node)))

(provide 'ego-functionality)
