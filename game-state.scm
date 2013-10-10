(define-module (square-man game-state)
  #:use-module (srfi srfi-9)
  #:use-module (square-man entity)
  #:export (<game-state>
	    make-game-state
	    game-state?
	    player
	    food
	    draw
	    update))

(define-record-type <game-state>
  (make-game-state player food)
  game-state?
  (player player)
  (food food))

(define (draw state)
  (draw-entity (player state))
  (for-each draw-entity (food state)))

(define (update state)
  (update-entity! (player state)))
