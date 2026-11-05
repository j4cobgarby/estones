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
  '((t :background "#e2b451" :foreground "#4f3c12"))
  "Face for Go board background and lines.")

;; Rendering Logic

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
        (pcase (ego-stone-at board x y)
          ('black (insert " B"))
          ('white (insert " w"))
          (_ (insert " ."))))
      (insert "\n"))
    (insert (format "To play: %s\n" (if last-move
                                        (ego-other-colour (ego-move-colour last-move))
                                      'black)))
    (let ((prisoners (ego-board-prisoners board)))
      (insert (format "Prisoners: (black) %d, (white) %d\n"
                    (car prisoners) (cdr prisoners))))))

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
