(load "entity.scm")
(load "game-state.scm")
(load "collision.scm")

(use-modules (srfi srfi-9)
	     (2d sprite)
             (2d game)
             (2d vector2)
	     (2d helpers)

	     (square-man entity)
	     (square-man collision)
	     (square-man game-state))

(define (player-sprite)
  (load-sprite "images/player.png"
               #:position (vector2 320 240)))

(define *player-speed* 1)

(define (key-down state key mod unicode)
  (cond ((any-equal? key 'a)
	 (move-left! (player state) *player-speed*))
	
	((any-equal? key 'd)
	 (move-right! (player state) *player-speed*))
	
	((any-equal? key 's)
	 (move-down! (player state) *player-speed*))
	
	((any-equal? key 'w)
	 (move-up! (player state) *player-speed*))))

(define-scene scene-1
  #:title  "Sqaure man scene"
  #:update update
  #:draw   draw
  #:events (append
	    (default-scene-events)
	    `((key-down . ,(lambda (state key mod unicode)
			     (key-down state key mod unicode)))))
  #:state  (make-game-state (make-entity (player-sprite) (vector2 0 0))))

(define-game square-man-game
  #:title       "Square Man"
  #:first-scene scene-1)

(run-game square-man-game)
