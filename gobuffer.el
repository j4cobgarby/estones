;; -*- lexical-binding: t; -*-

(require 'estones-functionality)

(defgroup estones nil
  "Play Go"
  :group 'games
  :prefix "estones-")

(defvar-local estones--game nil
  "The `estones-node' game tree root node that this buffer acts on")

(defvar-local estones--active-node nil
  "The currently active `estones-node' (within `estones--game')")

(defun estones--require-active-node ()
  "Either return current buffer's active node, or error"
  (or estones--active-node (user-error "No active Go node")))

(defun estones--require-game ()
  "Either return current buffer's Go game, or error"
  (or estones--game (user-error "No active Go game")))

(defface estones-board-face
  '((t :background "#e2b451" :foreground "#b78e37" :height 150))
  "Face for Go board background and lines.")

(defface estones-black-stone-face
  '((t :inherit estones-board-face :foreground "black"))
  "Face for black stones")

(defface estones-white-stone-face
  '((t :inherit estones-board-face :foreground "white"))
  "Face for white stones")

;;;; Rendering Logic

(defun star-offset (size) (if (>= size 13) 3 2))
(defun star-center? (size)
  (and (oddp size) (>= size 13)))
(defun star-middle? (size)
  (and (oddp size) (>= size 15)))

(defun is-star-point (board x y)
  (let* ((size (estones-board-size board))
         (lo (star-offset size))
         (hi (- size lo 1))
         (half (/ size 2)))
    (or (and (member x (list lo hi))
             (member y (list lo hi)))
        (and (star-center? size)
             (eql x half) (eql y half))
        (and (star-middle? size)
             (member x (list lo hi half))
             (member y (list lo hi half))))))

(defun estones-board-char (board x y)
  "Return the correct board markings for a given X Y position on BOARD"
  (let* ((size (estones-board-size board))
         (top (= y 0)) (bottom (= (1+ y) size))
         (left (= x 0)) (right (= (1+ x) size))
         (star-point))
    (propertize (cond
                 ((is-star-point board x y) "╋─")
                 ((and top left) "┌─")
                 ((and top right) "┐ ")
                 ((and bottom left) "└─")
                 ((and bottom right) "┘ ")
                 (top "┬─")
                 (bottom "┴─")
                 (left "├─")
                 (right "┤ ")
                 ("┼─"))
                'face 'estones-board-face
                'grid-idx (estones-idx board x y)
                'help-echo "Play a stone here.")))

(defun estones-stone-char (board x y)
    (let* ((size (estones-board-size board))
         (top (= y 0)) (bottom (= (1+ y) size))
         (left (= x 0)) (right (= (1+ x) size))
         (colour (estones-stone-at board x y)))
      (concat (propertize "●"
                          'face (if (eq colour 'black)
                                             'estones-black-stone-face
                                      'estones-white-stone-face)
                          'grid-idx (estones-idx board x y))
              (propertize (if right " " "─")
                          'face 'estones-board-face))))

(defun estones-render ()
  "Draw the current Go game into the buffer, at the state of the active node."
  (let* ((game (estones--require-game))
         (active-node (estones--require-active-node))
         (board (estones-node-board-snapshot active-node))
         (last-move (estones-node-move active-node))
         (size (estones-board-size board))
         (inhibit-read-only t)
         (save-point (point)))
    (erase-buffer)
    (dotimes (y size)
      (dotimes (x size)
        (insert (if (estones-stone-at board x y)
                    (estones-stone-char board x y)
                  (estones-board-char board x y))))
      (insert "\n"))
    (insert (format "To play: %s\n" (if last-move
                                        (estones-other-colour (estones-move-colour last-move))
                                      'black)))
    (let ((prisoners (estones-board-prisoners board)))
      (insert (format "Prisoners: (black) %d, (white) %d\n"
                    (car prisoners) (cdr prisoners))))
    (goto-char save-point)))

;;;; Mode stuff

(defvar-keymap estones-mode-map
  :parent special-mode-map
  "RET" #'estones-make-move-at-point
  "SPC" #'estones-make-move-at-point
  "<mouse-1>" #'estones-make-move-at-mouse)

(define-derived-mode estones-mode special-mode "Estones"
  "Major mode for playing Go.

\\{estones-mode-map}"
  (setq-local cursor-type 'box))

(defun estones-new-game (size)
  (interactive (list (read-number "Board size: " 19)))
  (let ((buf (generate-new-buffer (format "*go size=%d*" size))))
    (with-current-buffer buf
      (estones-mode)
      (setq estones--game (estones-new-gametree size 6.5))
      (setq estones--active-node estones--game)
      (estones-render))
    (pop-to-buffer-same-window buf)
    buf))

(defun estones-make-move (idx &optional colour)
  (interactive (list (read-number "Index: ")))
  (let ((node (estones--require-active-node)))
    (setq estones--active-node (estones-play-after-node node colour idx))
    (estones-render)))

(defun estones-make-move-at-point ()
  (interactive)
  (let ((idx (get-text-property (point) 'grid-idx)))
    (unless idx (user-error "Point must be over an intersection on the board"))
    (estones-make-move idx)))

(defun estones-make-move-at-mouse (event)
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (stone-idx (get-text-property pos 'grid-idx)))
    (unless stone-idx (user-error "Cannot play a stone here; not on board."))
    (estones-make-move stone-idx)))
