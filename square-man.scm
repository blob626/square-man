(use-modules (2d sprite)
	     (2d game)
             (2d window)
	     (2d math)
	     (2d coroutine)
	     (2d agenda)
	     (2d helpers)
	     (2d vector2)
	     (2d texture)
	     (2d actions)
	     (srfi srfi-9))

(define *window-width* 800)
(define *window-height* 600)

(define-record-type <entity>
  (make-entity sprite velocity)
  entity?
  (sprite %entity-sprite set-entity-sprite!)
  (velocity entity-velocity set-entity-velocity!))

(define (entity-sprite entity)
  ((%entity-sprite entity)))

(define (entity-position entity)
  (sprite-position (entity-sprite entity)))

(define (set-entity-position! entity value)
  (set-sprite-position! (entity-sprite entity) value))

(define (*player-texture*)
  (load-texture "images/player.png"))

;; Load a sprite and center it on the screen.
(define (*player-sprite*)
  (make-sprite (*player-texture*)
               #:position (vector2 (/ *window-width* 2)
				   (/ *window-height* 2))))

(define *player* (make-entity *player-sprite* (vector2 0 0)))

(define (*bullet-texture*) (load-texture "images/bullet.png"))

(define *bullets* (list))

(define (*enemy-texture*) (load-texture "images/enemy.png"))

(define *enemies* (list))

(define (shoot position)
  (set! *bullets*
	(append *bullets*
		(list (make-entity
		       (make-sprite (*bullet-texture*)
				    #:position position)
		       (vector2 0 -4))))))

(define (move-by! sprite delta)
  (set-sprite-position! sprite (v+ (sprite-position sprite) delta)))

(define (update-entity! entity)
  (move-by! (entity-sprite entity) (entity-velocity entity)))

(define (draw-entity entity)
  (draw-sprite (entity-sprite entity)))

(define (seconds->ticks seconds)
  (floor (* seconds 60)))

(define (v- . vectors)
  "subtracts vectors."
  (define (sub-vectors x y vectors)
    (cond ((null? vectors)
           (vector2 x y))
          (else
           (sub-vectors (- x (vx (car vectors)))
                        (- y (vy (car vectors)))
                        (cdr vectors)))))
  (sub-vectors (vx (car vectors))
	       (vy (car vectors)) (cdr vectors)))

(define (vector-towards origin target)
  (vnorm (v- (sprite-position origin) (sprite-position target))))

(define (cross a b)
  (vector2 (- (vy a) (vy b))
	   (- (vx b) (vx a))))

(define (rotate point angle)
  (vector2 (- (* (vx point) (cos angle))
	      (* (vy point) (sin angle)))
	   (+ (* (vx point) (sin angle))
	      (* (vy point) (cos angle)))))

(define (square x)
  (* x x))

(define (distance-square a b)
  (+ (square (- (vx a) (vx b)))
     (square (- (vy a) (vy b)))))

(define (closer-than origin target distance)
  (<= (distance-square origin target) (square distance)))

(define (vector-orbit origin target)
  (vnorm (cross (sprite-position origin)
		(sprite-position target))))

(define (velocity-towards origin target speed)
  (vscale (vector-towards target origin) speed))

(define (velocity-orbit origin target speed)
  (vscale (vector-orbit origin target) speed))

(define (velocity-coroutine origin function dt)
  (colambda ()
    (while #t
      (set-entity-velocity! origin (function))
      (wait dt))))

(define (enemy-movement origin target)
  (let ((radius 200) (speed 2))
    (define (in-orbit-range?)
      (closer-than (entity-position origin)
		   (entity-position target)
		   radius))
    
    (define (orbit-velocity)
      (velocity-orbit (entity-sprite origin)
		      (entity-sprite target) speed))
    
    (define (towards-velocity)
      (velocity-towards (entity-sprite origin)
			(entity-sprite target) speed))
    
    (define (velocity)
      (if (in-orbit-range?)
	  (orbit-velocity)
	  (towards-velocity)))

    (velocity-coroutine origin velocity 1)))

(define (orbit origin target)
  (define (velocity)
    (velocity-orbit (entity-sprite origin)
		    (entity-sprite target)
		    2))
  (velocity-coroutine origin velocity 10))

(define (follow origin target speed)
  "The origin entity moves towards the target entity at given speed"
  (define (velocity)
    (velocity-towards (entity-sprite origin)
		      (entity-sprite target) speed))

  (velocity-coroutine origin velocity 10))

(define (skew origin target)
  (define (velocity)
    (rotate (velocity-towards (entity-sprite origin)
			      (entity-sprite target) 2)
	    (degrees->radians 20)))
  (velocity-coroutine origin velocity (seconds->ticks 2)))

(define (spawn-enemy position)
  (let ((enemy (make-entity (make-sprite (*enemy-texture*)
					 #:position position)
			    (vector2 0 0))))
    (agenda-schedule
     (skew enemy *player*))
    (set! *enemies* (append *enemies* (list enemy)))))

(define (update-entities! entities)
  (map update-entity! entities))

(define (destroy-enemy enemy)
  (set! *enemies* (delete enemy *enemies*)))

(define (out-of-window? position)
  (or (> (vx position) *window-width*)
      (< (vx position) 0)
      (> (vy position) *window-height*)
      (< (vy position) 0)))

(define (delete item lst)
  (if (null? lst)
      '()
      (if (eqv? item (car lst))
	  (delete item (cdr lst))
	  (cons (car lst)
		(delete item (cdr lst))))))

(define (update-bullet! bullet)
  (if (out-of-window? (entity-position bullet))
      (set! *bullets* (delete bullet *bullets*))
      (update-entity! bullet)))

(define (update-bullets! bullets)
    (map update-bullet! bullets))

(define (make-move-entity player speed x y)
  (lambda ()
    (set-entity-velocity! player (vector2 (* speed x) (* speed y)))))

(define *speed* 1)

(define (make-move-player x y)
  (make-move-entity *player* *speed* x y))

(define move-player-left! (make-move-player -1 0))
(define move-player-right! (make-move-player 1 0))
(define move-player-up! (make-move-player 0 -1))
(define move-player-down! (make-move-player 0 1))

(define (key-down state key mod unicode)
  (cond ((any-equal? key 'a)
	 (set-entity-velocity! *player* (vector2 -1 0))
	 (move-player-left!))
	
	((any-equal? key 'd)
	 (move-player-right!))
	
	((any-equal? key 's)
	 (move-player-down!))
	
	((any-equal? key 'w)
	 (move-player-up!))
	
	((any-equal? key 'space)
	 (shoot (entity-position *player*)))
	
	((any-equal? key 'e)
	 (spawn-enemy (vector2 (- *window-width* 64)
			       64)))))

(define (update)
  (update-entity! *player*)
  (update-bullets! *bullets*)
  (update-entities! *enemies*))

(define *batch* (make-sprite-batch))

;; Draw our sprite
(define (render)
  (with-sprite-batch
   *batch*
   (for-each
    (lambda (sprite) (draw-entity sprite))
    (append (list *player*)
	    *bullets*
	    *enemies*))))

(define-scene square-scene
  #:title "Square"
  #:draw (lambda (state) (render))
  #:update (lambda (state) (update))
  #:events (append
	    (default-scene-events)
	    `((start . ,(lambda (state) "Start scene\n"))
	      (stop . ,(lambda (state) (display "stop scene\n")))
	      (key-down . ,(lambda (state key mod unicode)
			     (key-down state key mod unicode)))))
  #:state #f)

(define-game square-man
  #:title "Square-man"
  #:first-scene square-scene)

(run-game square-man)

