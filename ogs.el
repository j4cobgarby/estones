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

(defvar ogs-games (make-hash-table)
  "A map from OGS game ids to `estones-game' games.")

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
    (when (and (string-suffix-p "gamedata" event-name) (string-prefix-p "game/" event-name))
      (princ (format "Game data: %S\n" data))
      (setq ogs-last-game data))
    (pcase event-name
      ("net/pong" (transient-noop))
      ("active_game" (princ (format "Active game %d, on move %d: '%s'\n"
                                    (gethash "id" data)
                                    (gethash "move_number" data)
                                    (gethash "name" data))))
      (_ (princ (format "Unhandled '%s'\n" event-name))))))

(defun ogs-on-close (ws)
  (princ (format "Websocket closed at %s\n" (websocket-url ws))))

(defun ogs-on-error (ws which err)
  (princ (format "Websocket error from cbk %s: %s\n" which err)))

(defun ogs-connect (jwt)
  (setq ogs-ws (websocket-open ogs-ws-url
                               :on-open 'ogs-on-open
                               :on-message 'ogs-on-msg
                               :on-close 'ogs-on-close
                               :on-error 'ogs-on-error))
  (ogs-send-obj (vector "authenticate"
                        (list :jwt jwt)))
  (setq ogs-ping-timer (run-with-timer 10 10 'ogs-ping)))

(defun ogs-close-connection ()
  (cancel-timer ogs-ping-timer)
  (websocket-close ogs-ws))

;;; OGS Moves and Board

(defun ogs-play-gamedata (obj root)
  "Parses OBJ, which is a parsed JSON response from OGS representing a GobanEngineConfig structure (https://docs.online-go.com/goban/interfaces/GobanEngineConfig.html).
The moves are then played in order starting from ROOT.
The `estones-node' of the last move is returned."
  (let* ((moves (gethash "moves" obj))
         (leaf root))
    (seq-doseq (move moves)
      (princ (format "Move at %d, %d\n" (aref move 0) (aref move 1)))
      (setq leaf (estones-play-after-node leaf
                                (estones-next-move-colour
                                 (estones-node-move leaf))
                                (estones-idx (estones-node-board-snapshot leaf)
                                             (aref move 0) (aref move 1)))))
    leaf))
