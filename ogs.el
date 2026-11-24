;; -*- lexical-binding: t; -*-

(require 'websocket)
(require 'estones-functionality)

(defconst ogs-ws-url "wss://wss.online-go.com"
  "The URL to establish a websocket connection to OGS.")

(defconst ogs-rest-url "https://online-go.com/"
  "The base URL for normal GET requests to OGS.")

(defvar ogs-ws nil
  "Websocket connection to OGS.")

(defvar ogs-ping-timer nil
  "Timer to regularly ping OGS.")

;;; Global vars

(defvar ogs-login-id nil
  "The player id of the user logged in to OGS.")

(defvar ogs-games (make-hash-table)
  "A map from OGS game ids to `estones-game' games.")

(defvar ogs-last-moves (make-hash-table)
  "A map from OGS game ids to `estones-node's; the last played moves in those games.")

(defvar ogs-active-games (make-hash-table)
  "Info about all games the logged in user is in. Not just the ones that are connected to right now. Each key is an ID which maps to a list of (title, black, white, move-num).")

;;; Websocket helpers

(defun ogs-send-obj (obj)
  (let ((json-string (json-serialize obj)))
    (websocket-send-text ogs-ws json-string)))

(defun ogs-ping ()
  (ogs-send-obj (vector "net/ping" (list :client (floor (* (float-time) 1000))
                                         :drift 0 :latency 0))))

(defun ogs-game-connect (game-id)
  "Connect to an OGS game of id GAME-ID.
Once connected, we receive game updates."
  (ogs-send-obj (vector "game/connect" (list :game_id game-id))))

;;; Websocket callbacks

(defun ogs-on-open (ws)
  (princ (format "Successfully opened socket to %s\n" (websocket-url ws))))

(defun ogs-on-msg (ws frame)
  (let* ((txt (websocket-frame-text frame))
         (obj (json-parse-string txt))
         (event-name (aref obj 0))
         (data (aref obj 1)))
    ;; (when (and (string-suffix-p "gamedata" event-name) (string-prefix-p "game/" event-name))
    ;;   (princ (format "Game data: %S\n" data))
    ;;   (setq ogs-last-game data))
    (if (numberp event-name)
      ;; Here we have a response to an ID'd req.
        (pcase event-name
          (1337 (setq ogs-login-id (gethash "id" data))))
      (pcase (split-string event-name "/")
        ('("active_game") (ogs-handle-active-game data))
        (`("game" ,id "gamedata")
         (ogs-handle-gamedata (string-to-number id) data))
        (`("game" ,id "move")
         (ogs-handle-move (string-to-number id) data))
        (`("game" ,id "latency") (transient-noop))
        (`("game" ,id "clock") (transient-noop))
        ('("net" "pong") (transient-noop))
        ('("active-bots") (transient-noop))
        (_ (princ (format "Unhandled %s: %S\n" event-name data)))))))

(defun ogs-on-close (ws)
  (princ (format "Websocket closed at %s\n" (websocket-url ws))))

(defun ogs-on-error (ws which err)
  (princ (format "Websocket error from cbk %s: %s\n" which err)))

(defun ogs-connect (jwt)
  (interactive "sJWT string: ")
  (when (websocket-openp ogs-ws) (user-error "Already connected to OGS."))
  (setq ogs-ws (websocket-open ogs-ws-url
                               :on-open 'ogs-on-open
                               :on-message 'ogs-on-msg
                               :on-close 'ogs-on-close
                               :on-error 'ogs-on-error))
  (ogs-send-obj (vector "authenticate"
                        (list :jwt jwt)
                        1337))
  (setq ogs-ping-timer (run-with-timer 10 10 'ogs-ping))
  (setq ogs-games (make-hash-table)))

(defun ogs-close-connection ()
  (cancel-timer ogs-ping-timer)
  (websocket-close ogs-ws)
  (setq ogs-active-games (make-hash-table)
        ogs-games (make-hash-table)))

;;; OGS Games, Moves and Boards

(defun ogs-update-buffers-with-id (game-id new-active-node)
  (let ((game (gethash game-id ogs-games)))
    (estones-for-each-buffer
     (when (eq (estones--require-game) game)
       (setq estones--active-node new-active-node)
       (estones-render-current)))))

(defun ogs-handle-active-game (data)
  (puthash (gethash "id" data) data
           ogs-active-games))

(defun ogs-handle-move (game-id data)
  "Handle DATA received by a game/GAME-ID/move message from the server. Handling this just requires appending the move to the correct game tree."
  ;; Example move data:
  ;; ("game_id" 92668396 "move_number" 2 "move" [15 15 636103 :null #s(hash-table test equal data ("white_clock" #s(hash-table test equal data ("main_time" 604163897))))])
  (let* ((game (or (gethash game-id ogs-games)
                   (puthash game-id (ogs-game-from-gamedata data) ogs-games)))
         (move-number (gethash "move_number" data))
         (move-vec (gethash "move" data))
         (x (aref move-vec 0))
         (y (aref move-vec 1))
         (idx (estones-idx (estones-node-board-snapshot (estones-game-root game))
                           x y))
         (last-move (gethash game-id ogs-last-moves)))
    (princ (format "Got move %d for game %d @ %d, %d"
                   move-number game-id x y))
    (unless (= move-number (1+ (estones-node-move-number last-move)))
      (user-error "Move number mismatch! Maybe a message was missed..."))
    (let ((new-node (estones-play-after-node last-move nil idx)))
      (puthash game-id new-node ogs-last-moves)
      (setf (estones-game-main-leaf game) new-node)
      (ogs-update-buffers-with-id game-id new-node))))

(defun ogs-handle-gamedata (game-id data)
  "Handle the DATA received by a game/GAME-ID/gamedata message from the server.

Receiving such a message implies that this is an active game. DATA, which is expected to represent a GobanEngineConfig as a hashmap, is used to update the game representation in `ogs-games'. If no game exists in `ogs-games' with GAME-ID, it is created anew."
  (let* ((game (or (gethash game-id ogs-games)
                   (puthash game-id (ogs-game-from-gamedata data) ogs-games)))
         (srv-moves (gethash "moves" data))
         (pointer (estones-game-root game)))
    ;; Here we iterate over the list of moves that OGS sees, ensure that those moves exist in our game tree, and also that they are the main line.
    (seq-doseq (raw-move srv-moves)
      (princ (format "Handling move %S\n" raw-move))
      (let* ((x (aref raw-move 0))
             (y (aref raw-move 1))
             (idx (estones-idx (estones-node-board-snapshot pointer) x y))
             (new-node (estones-play-after-node pointer nil idx)))
        (princ (format "Played move at idx %d\n" idx))
        (setq pointer new-node)))
    (setf (estones-game-main-leaf game) pointer)
    (ogs-update-buffers-with-id game-id pointer)
    (puthash game-id pointer ogs-last-moves)))

(defun ogs-game-from-gamedata (data)
  "Make a blank `estones-game' with metadata from DATA. Only the metadata is filled in, but the moves from DATA are not played out."
  (estones-game--make
   :root (estones-new-gametree (gethash "width" data)
                               (gethash "komi" data))
   :black-name (gethash "black_player_id" data)
   :white-name (gethash "white_player_id" data)
   :title (gethash "game_name" data)
   :black-is-local (eql ogs-login-id
                        (gethash "black_player_id" data))
   :white-is-local (eql ogs-login-id
                        (gethash "white_player_id" data))
   :has-ended (equal "ended" (gethash "phase" data))))

(defun ogs-game-from-active-game (id data)
  (let ((new-game (estones-game--make
          :root (estones-new-gametree (gethash "width" data)
                                      (or (gethash "komi" data) 6.5))
          :black-name (gethash "username" (gethash "black" data))
          :white-name (gethash "username" (gethash "white" data))
          :title (gethash "name" data)
          :black-is-local (eql ogs-login-id (gethash "id" (gethash "black" data)))
          :white-is-local (eql ogs-login-id (gethash "id" (gethash "white" data)))
          :has-ended (equal "ended" (gethash "phase" data)))))
    (puthash id new-game ogs-games)
    (ogs-game-connect id)
    new-game))

(defun ogs-make-move (idx game-id)
  (let* ((game (gethash game-id ogs-games))
         (node (estones-game-main-leaf game))
         (board (estones-node-board-snapshot node))
         (coords (estones-coords board idx)))
    (unless (estones-local-to-playp game)
      (user-error "It's not your turn"))
    (princ (format "Playing a move in game %d at %s\n" game-id coords))
    (ogs-send-obj (vector "game/move" (list :game_id game-id :move coords)))))

;;; Buffer/mode related stuff

(defun ogs-game-buffer (id &optional jump-to-end)
  "This function opens a buffer in which an OGS game can be played. ID is the OGS id of the game, and if JUMP-TO-END, then the buffer view starts at the end of the game."
  ;; If the game is already in `ogs-games', its state is already loaded, and an estones game buffer can directly be made.
  ;; Otherwise, the game may be in `ogs-active-games' if it's a game that the user is a player in. In such a case, we can for now at least create an empty game with the correct metadata (such as board size), and send a `game/connect' to OGS to populate the game with the real moves.
  ;; Otherwise, it may be a real OGS game which the user is not actually in. That's still fine to join as a spectator. In this case, though, we don't yet have any metadata about it, so the best we can do is make a properly blank game, and guess the board size as 19x19.
  (let* ((connected-game (gethash id ogs-games))
         (active-game-info (gethash id ogs-active-games))
         (game (or connected-game
                   (ogs-game-from-active-game id active-game-info)
                   (user-error "Cannot spectate non-active games for now..."))))
    (with-current-buffer (estones-game-buffer game jump-to-end)
      (setq ogs-buffer-game-id id)
      (setq estones-make-move-lambda (lambda (idx)
                                       (ogs-make-move idx id))))))

(defun estones-lobby--ogs-links ()
  (if (websocket-openp ogs-ws)
      (progn
        (estones-lobby--list-link "(Log out)"
                                  (lambda () (ogs-close-connection)))
        (estones-lobby--list-link "Auto-matchmaking"
                                  (lambda () (princ "Not implemented")))
        (dolist (game-id (hash-table-keys ogs-active-games))
          (let* ((game-list-info (gethash game-id ogs-active-games))
                 (game-state (gethash game-id ogs-games)))
            (estones-lobby--list-link (gethash "name" game-list-info)
                                      (lambda () (ogs-game-buffer game-id t))
                                      (format "move %d\t%s VS %s"
                                              (gethash "move_number" game-list-info)
                                              (gethash "username" (gethash "black" game-list-info))
                                              (gethash "username" (gethash "white" game-list-info)))))))
    (estones-lobby--list-link "Log in"
                              (lambda () (call-interactively #'ogs-connect)))))
(push (cons "OGS Games" #'estones-lobby--ogs-links)
      estones-lobby-functions)
