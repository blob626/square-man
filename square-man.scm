(load "entity.scm")

(use-modules (srfi srfi-9)
	     (2d sprite)
             (2d game)
             (2d vector2)
	     (2d helpers)

	     (square-man entity))

(define-record-type <game-state>
  (make-game-state player)
  game-state?
  (player game-player))

(define (player-sprite)
  (load-sprite "images/player.png"
               #:position (vector2 320 240)))

(define (draw state)
  (draw-entity (game-player state)))

(define (update state)
  (update-entity! (game-player state)))

(define (make-move-entity x y)
  (lambda (entity speed)
    (set-entity-velocity! entity (vector2 (* speed x) (* speed y)))))

(define move-left! (make-move-entity -1 0))
(define move-right! (make-move-entity 1 0))
(define move-up! (make-move-entity 0 -1))
(define move-down! (make-move-entity 0 1))

(define *player-speed* 1)

(define (key-down state key mod unicode)
  (cond ((any-equal? key 'a)
	 (move-left! (game-player state) *player-speed*))
	
	((any-equal? key 'd)
	 (move-right! (game-player state) *player-speed*))
	
	((any-equal? key 's)
	 (move-down! (game-player state) *player-speed*))
	
	((any-equal? key 'w)
	 (move-up! (game-player state) *player-speed*))))

(define-scene demo
  #:title  "Demo"
  #:update (lambda (state) (update state))
  #:draw   (lambda (state) (draw state))
  #:events (append
	    (default-scene-events)
	    `((key-down . ,(lambda (state key mod unicode)
			     (key-down state key mod unicode)))))
  #:state  (make-game-state (make-entity (player-sprite) (vector2 0 0))))

(define-game simple
  #:title       "Simple Demo"
  #:first-scene demo)

(run-game simple)
