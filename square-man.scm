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

(define *screen-width* 640)
(define *screen-height* 480)

(define (player-sprite)
  (load-sprite "images/player.png"
               #:position (vector2 320 240)))

(define (food-sprite position)
  (load-sprite "images/bullet.png"
	       #:position position))

(define grid (make-grid *screen-height* *screen-width* 64))

(define (collide? a b)
  (< (distance-square (entity-position a) (entity-position b))
     (square (+ (entity-radius a) (entity-radius b)))))

(define (collision-handler collision)
  (define (check-type? type-a type-b)
    (lambda (a b)
      (or (and (eqv? (entity-type a) type-a)
	       (eqv? (entity-type b) type-b))
	  (and (eqv? (entity-type a) type-b)
	       (eqv? (entity-type b) type-a)))))
  (let ((a (car collision))
	(b (cadr collision)))
    (cond (((check-type? 'food 'player) a b) (quit)))))

(define (state-objects state)
  (cons (player state) (food state)))

(define collision-system
  (make-collision-system state-objects grid collide? collision-handler))

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
  #:update (lambda (state)
	     (update state)
	     (collision-system state))
  #:draw   draw
  #:events (append
	    (default-scene-events)
	    `((key-down . ,(lambda (state key mod unicode)
			     (key-down state key mod unicode)))))
  #:state  (make-game-state
	    (make-entity 'player
			 (player-sprite) 16 (vector2 0 0))
	    (list (make-entity 'food
			       (food-sprite (vector2 10 10))
			       10
			       (vector2 0 0)))))

(define-game square-man-game
  #:title       "Square Man"
  #:resolution (vector2 *screen-width* *screen-height*)
  #:first-scene scene-1)

(run-game square-man-game)
