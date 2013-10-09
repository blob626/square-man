(define-module (square-man game-state)
  #:use-module (srfi srfi-9)
  #:use-module (square-man entity)
  #:export (<game-state>
	    make-game-state
	    game-state?
	    player
	    draw
	    update))

(define-record-type <game-state>
  (make-game-state player)
  game-state?
  (player player))

(define (draw state)
  (draw-entity (player state)))

(define (update state)
  (update-entity! (player state)))
