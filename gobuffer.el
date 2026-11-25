;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'estones-functionality)

(defgroup estones nil
  "Play Go"
  :group 'games
  :prefix "estones-")

(defvar-local estones--game nil
  "The `estones-game' that this buffer represents")

(defvar-local estones--active-node nil
  "The currently active `estones-node' (within `estones--game's tree.)")

(defun estones--require-active-node ()
  "Either return current buffer's active node, or error"
  (or estones--active-node (user-error "No active Go node")))

(defun estones--require-game ()
  "Either return current buffer's Go game, or error"
  (or estones--game (user-error "No active Go game")))

(defface estones-board-face
  '((t :background "#e2b451" :foreground "#b78e37"))
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

(defun estones-render (game active-node)
  "Mainly called by `estones-render-current' to draw a Go board state."
  (let* ((board (estones-node-board-snapshot active-node))
         (last-move (estones-node-move active-node))
         (size (estones-board-size board))
         (inhibit-read-only t)
         (save-point (point)))
    (erase-buffer)
    ;; Game description
    (insert (format "'%s'\n------\n" (estones-game-title game)))
    (insert (format "%s (black) VS %s (white)\n"
                    (estones-game-black-name game)
                    (estones-game-white-name game)))
    (dotimes (y size)
      ;; Row labels
      (insert (propertize
               (format "%2d " (- (estones-board-size board) y))
               'face 'estones-board-face))
      ;; Board
      (dotimes (x size)
        (insert (if (estones-stone-at board x y)
                    (estones-stone-char board x y)
                  (estones-board-char board x y))))
      (insert "\n"))
    ;; Column labels
    (insert (propertize "   " 'face 'estones-board-face))
    (dotimes (x size)
      (insert (propertize
               (format "%c " (aref estones-columns x))
               'face 'estones-board-face)))
    (insert "\n")
    ;; Game status
    (let ((siblings (estones--siblings active-node)))
      (insert "Siblings: ")
      (dolist (sib siblings)
        (if (eq sib active-node) (insert "->"))
        (insert (estones-coords board
                                (estones-move-idx (estones-node-move sib))))
        (insert "  "))
      (insert "\n"))
    (insert (format "Move %d -- "
                    (estones-node-move-number active-node)))
    (insert (format "%s to play \n" (if last-move
                                        (estones-other-colour (estones-move-colour last-move))
                                      'black)))
    (let ((prisoners (estones-board-prisoners board)))
      (insert (format "Prisoner points: (black) %d | (white) %d\n"
                      (car prisoners) (cdr prisoners))))
    (when-let* ((children (estones-node-children active-node))
                (mainline (estones-node-main-move active-node)))
      (insert "\nContinuations: ")
      (dolist (child children)
        (if (eq child mainline) (insert "->"))
        (insert (estones-coords board
                                (estones-move-idx (estones-node-move child))))
        (insert "  ")))
    (goto-char save-point)))

(defun estones-render-current ()
  (estones-render (estones--require-game) (estones--require-active-node)))

;;;; Mode stuff

(defvar-keymap estones-mode-map
  :parent special-mode-map
  "RET" #'estones-make-move-at-point
  "SPC" #'estones-make-move-at-point
  "M-n" #'estones-seek-forward
  "M-p" #'estones-seek-back
  "M-f" #'estones-seek-sibling
  "M-b" #'estones-seek-sibling-back
  "<mouse-1>" #'estones-make-move-at-mouse)

(define-derived-mode estones-mode special-mode "Estones"
  "Major mode for playing Go.

\\{estones-mode-map}"
  (setq-local cursor-type 'hollow))

(defun estones-game-buffer (game &optional jump-to-end)
  
  (let ((buf (generate-new-buffer (format "*Go '%s'*" (estones-game-title game)))))
    (with-current-buffer buf
      (estones-mode)
      (setq estones--game game)
      (setq estones--active-node (if jump-to-end
                                     (estones-game-main-leaf estones--game)
                                   (estones-game-root estones--game)))
      (estones-render-current))
    (pop-to-buffer-same-window buf)
    buf))

(defvar-local estones-make-move-lambda
  (lambda (idx) (estones-make-move-directly idx))
  "This is called when a user interactively tries to play a move in an estones buffer. The purpose is so that external Go game interfaces (like OGS) can be implemented.
It should be a function which takes one argument: the index to play a stone at.
It should then do something which amounts to playing that move after `estones--active-node' and updating `estones--game' (and probably `estones--active-node'.")

(defun estones-make-move-directly (idx &optional colour)
  "Directly make a move from the currently active node."
  (interactive (list (read-number "Index: ")))
  (let ((node (estones--require-active-node)))
    (setq estones--active-node (estones-play-after-node node colour idx))
    (estones-render-current)))

(defun estones-make-move-at-point ()
  (interactive)
  (let ((idx (get-text-property (point) 'grid-idx)))
    (unless idx (user-error "Point must be over an intersection on the board"))
    (funcall estones-make-move-lambda idx)))

(defun estones-make-move-at-mouse (event)
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (stone-idx (get-text-property pos 'grid-idx)))
    (unless stone-idx (user-error "Cannot play a stone here; not on board."))
    (funcall estones-make-move-lambda stone-idx)))

(defmacro estones-for-each-buffer (body)
  "Do BODY with each buffer that's in `estones-mode'."
  `(dolist (buf (buffer-list))
     (with-current-buffer buf
       (when (eq major-mode 'estones-mode)
         ,body))))

;;; Navigating through game

(defun estones-seek-to (node)
  (setq estones--active-node node)
  (estones-render-current))

(defun estones-seek-back ()
  (interactive)
  (estones-seek-to
   (or (estones-node-parent estones--active-node)
       estones--active-node)))

(defun estones-seek-forward ()
  (interactive)
  (estones-seek-to
   (or (estones-node-main-move estones--active-node)
       estones--active-node)))

(defun estones--siblings (node)
  "Return all of NODE's siblings, including itself"
  (let ((parent (estones-node-parent node)))
    (if parent
        (estones-node-children parent)
      (list node))))

(defun estones--sibling (node offset)
  "Return a sibling of NODE that is the OFFSET'th next in the list."
  (let* ((children (estones--siblings node))
         (idx (cl-position node children))
         (num-children (length children)))
    (nth (mod (+ idx offset) num-children) children)))

(defun estones-seek-sibling (offset)
  (interactive "p")
  (estones-seek-to
   (estones--sibling (estones--require-active-node)
                     offset)))

(defun estones-seek-sibling-back (offset)
  (interactive "p")
  (estones-seek-to
   (estones--sibling (estones--require-active-node)
                     (- offset))))

;;; Lobby/Entering games

(define-derived-mode estones-lobby-mode special-mode "Estones Lobby"
  "Mode for entering games of go.")

(define-key estones-lobby-mode-map "g" #'estones-lobby)

(defun estones-lobby--list-link (lbl fn &optional description)
  (insert " > ")
  (insert-text-button lbl
                      'action (lambda (_btn) (funcall fn))
                      'follow-link t)
  (insert "\n")
  (when description
    (insert (concat "    - " description "\n"))))

(defun estones-lobby--sep ()
  (insert "  ------\n"))

(defun estones-lobby--local-links ()
  (estones-lobby--list-link "New local game"
                            (lambda () (call-interactively #'estones-new-local-game)))
  (estones-lobby--list-link "Load SGF file"
                            (lambda () (message "Not implemented"))))

(defvar estones-lobby-functions
  (list (cons "Local Games" #'estones-lobby--local-links))
  "List of (label . function), defining each section in the Estones lobby. The function should print out some links to start/join whatever games (with `estones-lobby--list-link'")

(defun estones-lobby ()
  "Show the Estones lobby."
  (interactive)
  (let ((buf (get-buffer-create "*Estones*")))
    (with-current-buffer buf
      (estones-lobby-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "╔═╗┌─┐┌┬┐┌─┐┌┐┌┌─┐┌─┐
║╣ └─┐ │ │ ││││├┤ └─┐
╚═╝└─┘ ┴ └─┘┘└┘└─┘└─┘
Stones for Emacs!\n")
        (dolist (section estones-lobby-functions)
          (let ((lbl (car section))
                (func (cdr section)))
            (insert (concat "\n" lbl "\n"))
            (funcall func))))
      (goto-char (point-min))
      (forward-button 1))
    (pop-to-buffer buf '(display-buffer-same-window . ()))))
