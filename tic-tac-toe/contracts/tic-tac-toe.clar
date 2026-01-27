;; ============================================
;; Tic Tac Toe Smart Contract (Clarity 3)
;; ============================================

;; --------------------
;; Constants & Errors
;; --------------------

(define-constant ERR_MIN_BET_AMOUNT (err u100))
(define-constant ERR_INVALID_MOVE (err u101))
(define-constant ERR_GAME_NOT_FOUND (err u102))
(define-constant ERR_GAME_CANNOT_BE_JOINED (err u103))
(define-constant ERR_NOT_YOUR_TURN (err u104))
(define-constant ERR_GAME_OVER (err u105))
(define-constant ERR_PLAYER_MISSING (err u106))

(define-constant MARK_EMPTY u0)
(define-constant MARK_X u1)
(define-constant MARK_O u2)

(define-constant STATUS_WAITING u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_FINISHED u2)

;; --------------------
;; Storage
;; --------------------

(define-data-var latest-game-id uint u0)

(define-map games
    { id: uint }
    {
        player-one: principal,
        player-two: (optional principal),
        turn: principal,
        bet: uint,
        board: (list 9 uint),
        status: uint,
        winner: (optional principal),
    }
)

;; --------------------
;; Helpers
;; --------------------

(define-private (empty-board)
    (list
        MARK_EMPTY         MARK_EMPTY         MARK_EMPTY
        MARK_EMPTY         MARK_EMPTY         MARK_EMPTY
        MARK_EMPTY         MARK_EMPTY         MARK_EMPTY
    )
)

(define-private (valid-move?
        (board (list 9 uint))
        (i uint)
    )
    (and
        (< i u9)
        (is-eq (unwrap! (element-at? board i) false) MARK_EMPTY)
    )
)

(define-private (is-line
        (board (list 9 uint))
        (a uint)
        (b uint)
        (c uint)
    )
    (let (
            (av (unwrap! (element-at? board a) false))
            (bv (unwrap! (element-at? board b) false))
            (cv (unwrap! (element-at? board c) false))
        )
        (and
            (is-eq av bv)
            (is-eq av cv)
            (not (is-eq av MARK_EMPTY))
        )
    )
)

(define-private (has-won (board (list 9 uint)))
    (or
        (is-line board u0 u1 u2)
        (is-line board u3 u4 u5)
        (is-line board u6 u7 u8)
        (is-line board u0 u3 u6)
        (is-line board u1 u4 u7)
        (is-line board u2 u5 u8)
        (is-line board u0 u4 u8)
        (is-line board u2 u4 u6)
    )
)

(define-private (board-full? (board (list 9 uint)))
    (is-none (index-of board MARK_EMPTY))
)

;; --------------------
;; Public Functions
;; --------------------

(define-public (create-game
        (bet uint)
        (move-index uint)
    )
    (begin
        (asserts! (> bet u0) ERR_MIN_BET_AMOUNT)
        (asserts! (valid-move? (empty-board) move-index) ERR_INVALID_MOVE)

        (try! (as-contract (stx-transfer? bet tx-sender tx-sender)))

        (let (
                (id (var-get latest-game-id))
                (board (unwrap! (replace-at? (empty-board) move-index MARK_X)
                    ERR_INVALID_MOVE
                ))
            )
            (map-set games { id: id } {
                player-one: tx-sender,
                player-two: none,
                turn: tx-sender,
                bet: bet,
                board: board,
                status: STATUS_WAITING,
                winner: none,
            })

            (var-set latest-game-id (+ id u1))

            (print {
                event: "game-created",
                game-id: id,
                player: tx-sender,
                bet: bet,
            })

            (ok id)
        )
    )
)

(define-public (join-game
        (game-id uint)
        (move-index uint)
    )
    (let ((g (unwrap! (map-get? games { id: game-id }) ERR_GAME_NOT_FOUND)))
        (asserts! (is-eq (get status g) STATUS_WAITING) ERR_GAME_CANNOT_BE_JOINED)
        (asserts! (is-none (get player-two g)) ERR_GAME_CANNOT_BE_JOINED)
        (asserts! (valid-move? (get board g) move-index) ERR_INVALID_MOVE)

        (try! (as-contract (stx-transfer? (get bet g) tx-sender tx-sender)))

        (let ((board (unwrap! (replace-at? (get board g) move-index MARK_O)
                ERR_INVALID_MOVE
            )))
            (map-set games { id: game-id }
                (merge g {
                    player-two: (some tx-sender),
                    board: board,
                    turn: (get player-one g),
                    status: STATUS_ACTIVE,
                })
            )

            (print {
                event: "game-joined",
                game-id: game-id,
                player: tx-sender,
            })

            (ok game-id)
        )
    )
)

(define-public (play
        (game-id uint)
        (move-index uint)
    )
    (let ((g (unwrap! (map-get? games { id: game-id }) ERR_GAME_NOT_FOUND)))
        (asserts! (is-eq (get status g) STATUS_ACTIVE) ERR_GAME_OVER)
        (asserts! (is-eq tx-sender (get turn g)) ERR_NOT_YOUR_TURN)
        (asserts! (valid-move? (get board g) move-index) ERR_INVALID_MOVE)

        (let ((mark (if (is-eq tx-sender (get player-one g))
                MARK_X
                MARK_O
            )))
            (let ((board (unwrap! (replace-at? (get board g) move-index mark)
                    ERR_INVALID_MOVE
                )))
                (let ((won (has-won board)))
                    (let ((draw (and (not won) (board-full? board))))
                        (let ((next (if (is-eq tx-sender (get player-one g))
                                (unwrap! (get player-two g) ERR_PLAYER_MISSING)
                                (get player-one g)
                            )))
                            (map-set games { id: game-id }
                                (merge g {
                                    board: board,
                                    turn: next,
                                    status: (if (or won draw)
                                        STATUS_FINISHED
                                        STATUS_ACTIVE
                                    ),
                                    winner: (if won
                                        (some tx-sender)
                                        none
                                    ),
                                })
                            )

                            ;; payout
                            (if won
                                (try! (as-contract (stx-transfer? (* u2 (get bet g)) tx-sender
                                    tx-sender
                                )))
                                (if draw
                                    (begin
                                        (try! (as-contract (stx-transfer? (get bet g) tx-sender
                                            (get player-one g)
                                        )))
                                        (try! (as-contract (stx-transfer? (get bet g) tx-sender
                                            (unwrap! (get player-two g)
                                                ERR_PLAYER_MISSING
                                            ))))
                                        true
                                    )
                                    true
                                )
                            )

                            (print {
                                event: "move-played",
                                game-id: game-id,
                                player: tx-sender,
                                won: won,
                                draw: draw,
                            })

                            (ok game-id)
                        )
                    )
                )
            )
        )
    )
)

;; --------------------
;; Read-only
;; --------------------

(define-read-only (get-game (game-id uint))
    (map-get? games { id: game-id })
)

(define-read-only (get-latest-game-id)
    (var-get latest-game-id)
)
