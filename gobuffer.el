;; -*- lexical-binding: t; -*-

(require 'ego-functionality)

(defgroup ego nil
  "Play Go"
  :group 'games
  :prefix "ego-")

(defvar-local ego--game nil
  "The `ego-node' game tree root node that this buffer acts on")

(defvar-local ego--active-node nil
  "The currently active `ego-node' (within `ego--game')")

(defun ego--require-active-node ()
  "Either return current buffer's active node, or error"
  (or ego--active-node (user-error "No active Go node")))

(defun ego--require-game ()
  "Either return current buffer's Go game, or error"
  (or ego--game (user-error "No active Go game")))

(defface ego-board-face
  '((t :background "#e2b451" :foreground "#b78e37" :height 150))
  "Face for Go board background and lines.")

(defface ego-black-stone-face
  '((t :inherit ego-board-face :foreground "black"))
  "Face for black stones")

(defface ego-white-stone-face
  '((t :inherit ego-board-face :foreground "white"))
  "Face for white stones")

;;;; Rendering Logic

(defun ego-board-char (board x y)
  "Return the correct board markings for a given X Y position on BOARD"
  (let* ((size (ego-board-size board))
         (top (= y 0)) (bottom (= (1+ y) size))
         (left (= x 0)) (right (= (1+ x) size)))
    (propertize (cond ((and top left) "┌─")
          ((and top right) "┐ ")
          ((and bottom left) "└─")
          ((and bottom right) "┘ ")
          (top "┬─")
          (bottom "┴─")
          (left "├─")
          (right "┤ ")
          ("┼─"))
                'face 'ego-board-face
                'grid-idx (ego-idx board x y))))

(defun ego-stone-char (board x y)
    (let* ((size (ego-board-size board))
         (top (= y 0)) (bottom (= (1+ y) size))
         (left (= x 0)) (right (= (1+ x) size))
         (colour (ego-stone-at board x y)))
      (concat (propertize "●"
                          'face (if (eq colour 'black)
                                             'ego-black-stone-face
                                      'ego-white-stone-face)
                          'grid-idx (ego-idx board x y))
              (propertize (if right " " "─")
                          'face 'ego-board-face
                          'grid-idx (ego-idx board x y)))))

(defun ego-render ()
  "Draw the current Go game into the buffer, at the state of the active node."
  (let* ((game (ego--require-game))
         (active-node (ego--require-active-node))
         (board (ego-node-board-snapshot active-node))
         (last-move (ego-node-move active-node))
         (size (ego-board-size board))
         (inhibit-read-only t))
    (erase-buffer)
    (dotimes (y size)
      (dotimes (x size)
        (insert (if (ego-stone-at board x y)
                    (ego-stone-char board x y)
                  (ego-board-char board x y))))
      (insert "\n"))
    (insert (format "To play: %s\n" (if last-move
                                        (ego-other-colour (ego-move-colour last-move))
                                      'black)))
    (let ((prisoners (ego-board-prisoners board)))
      (insert (format "Prisoners: (black) %d, (white) %d\n"
                    (car prisoners) (cdr prisoners))))))

;;;; Mode stuff

(define-derived-mode go-mode special-mode "Go"
  "Major mode for playing Go."
  (setq-local cursor-type 'box))

(defun go-new-game (size)
  (interactive (list (read-number "Board size: " 19)))
  (let ((buf (generate-new-buffer (format "*Go board*"))))
    (with-current-buffer buf
      (go-mode)
      (setq ego--game (ego-new-gametree size 6.5))
      (setq ego--active-node ego--game)
      (ego-render))
    (pop-to-buffer-same-window buf)
    buf))

(defun go-play (color idx)
  (interactive
   (let* ((node (ego--require-active-node))
         (board (ego-node-board-snapshot node))
         (last-move (ego-node-move node)))
     (list (ego-other-colour (if last-move
                                 (ego-move-colour last-move)
                               'white))
           (read-number "Index: "))))
  (let ((node (ego--require-active-node)))
    (setq ego--active-node (ego-play-after-node node color idx))
    (ego-render)))
