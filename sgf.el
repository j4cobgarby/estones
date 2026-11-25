;;; -*- lexical-binding: t; -*-

;;; Parser for, and generator of, Simple Game Format (SGF) files
;;; Following https://homepages.cwi.nl/~aeb/go/misc/sgf.html

(require 'estones-functionality)

(defun sgf--skip-ws ()
  "Move point forward to pass sequence of whitespace"
  (skip-chars-forward " \t\r\n"))

(defun sgf--parse-prop-value ()
  "Parse a PropValue at point, returning the inner value as a string"
  (unless (= ?\[ (char-after)) (error "Expected [ at start of prop"))
  (forward-char)
  (let (chars)
    (while (not (eq ?\] (char-after)))
      (when (eobp)
        (user-error "Unterminated property"))
      (when (eq ?\\ (char-after))
        (forward-char))
      (push (char-after) chars)
      (forward-char))
    (forward-char)
    (concat (nreverse chars))))

(defun sgf--parse-prop-ident ()
  "Parse a PropIdent, returning it as a string"
  (let ((start (point)))
    (when (= 0 (skip-chars-forward "A-Z"))
      (user-error "PropIdent expects chars in A-Z"))
    (buffer-substring-no-properties start (point))))

(defun sgf--parse-property ()
  "Parse a Property at point, returning a list of the form (IDENT, VALUE, ...)"
  (let ((ident (sgf--parse-prop-ident))
        (values))
    (sgf--skip-ws)
    (while (eq ?\[ (char-after))
      (push (sgf--parse-prop-value) values)
      (sgf--skip-ws))
    (unless values (user-error "No values found for ident %s" ident))
    (cons ident (nreverse values))))

(defun sgf--parse-node ()
  "Parse a Node at point, returning a list (PROPERTY, ...)."
  (unless (eq ?\; (char-after)) (user-error "Expected ; to start a node"))
  (forward-char)
  (let ((props))
    (while (<= ?A (char-after) ?Z)
      (push (sgf--parse-property) props)
      (sgf--skip-ws))
    (cons :node (nreverse props))
    ))

(defun sgf--parse-game-tree ()
  "Parse a GameTree, as nested lists."
  (unless (eq ?\( (char-after)) (user-error "Game tree should start with ("))
  (forward-char)
  (let ((tree))
    (while (not (eq ?\) (char-after)))
      (push (pcase (char-after)
              (?\( (sgf--parse-game-tree))
              (?\; (sgf--parse-node)))
            tree))
    (forward-char)
    (cons :tree (nreverse tree))))

(defun sgf--colour (colour-string)
  (pcase colour-string
    ("B" 'black)
    ("W" 'white)
    (_ (user-error "Invalid SGF colour string %s" colour-string))))

(defun sgf--idx (board-size coord-string)
  "Convert an SGF move coordinate string to an estones index. If COORD-STRING is empty, interpret it as a pass, and return nil."
  (if (string-empty-p coord-string)
      nil
    (let ((x (- (aref coord-string 0) ?a))
          (y (- (aref coord-string 1) ?a)))
      (estones-idx board-size x y))))

(defun sgf--node-prop1 (node ident)
  "First property in an ident's property list within a node."
  (car (cdr (assoc ident node))))

(defun sgf--play-from (sgf game-tree)
  "Play moves from an SGF starting at the node GAME-TREE. This is a recursive function."
  (let ((board-size (estones-board-size (estones-node-board-snapshot game-tree))))
    (pcase (car sgf)
      (:tree
       (princ (format "Doing tree: %S\n" (cdr sgf)))
       (let ((prev-tree game-tree))
         (dolist (variation (cdr sgf))
           (setq game-tree (sgf--play-from variation game-tree)))
         prev-tree))
      (:node (let ((black-move (sgf--node-prop1 (cdr sgf) "B"))
                   (white-move (sgf--node-prop1 (cdr sgf) "W")))
               (princ (format "Black move: %S, white move: %S\n" black-move white-move))
               (if black-move
                   (estones-play-after-node game-tree 'black
                                          (sgf--idx board-size black-move))
                 (if white-move
                     (estones-play-after-node game-tree 'white
                                          (sgf--idx board-size white-move))
                   game-tree)))))))

(defun sgf--build-game (sgf)
  "Build an estones game from an SGF. SGF is the return value from `sgf--parse-game-tree'"
  (let* ((root-node (car (cdr sgf)))
         (rest (cdr sgf))
         (game (estones-game--make
                :root (estones-new-gametree
                       (or (string-to-number (sgf--node-prop1 root-node "SZ"))
                           (user-error "No size (SZ) given."))
                       (or (string-to-number (sgf--node-prop1 root-node "KM")) 0))
                :black-name (sgf--node-prop1 root-node "PB")
                :white-name (sgf--node-prop1 root-node "PW")
                :title (sgf--node-prop1 root-node "GN")
                :black-is-local t :white-is-local t)))
    (sgf--play-from sgf (estones-game-root game))))
