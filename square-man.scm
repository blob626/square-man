(use-modules (2d sprite)
             (2d game-loop)
             (2d window)
	     (2d helpers)
	     (2d vector2)
	     (2d texture)
	     (2d actions)
	     (srfi srfi-9))

(define *window-width* 800)
(define *window-height* 600)

;; Open the window.
(open-window *window-width* *window-height*)

(define-record-type <entity>
  (make-entity sprite velocity)
  entity?
  (sprite entity-sprite set-entity-sprite!)
  (velocity entity-velocity set-entity-velocity!))

(define (entity-position entity)
  (sprite-position (entity-sprite entity)))

(define (set-entity-position! entity value)
  (set-sprite-position! (entity-sprite entity) value))

;; Load a sprite and center it on the screen.
(define *player-sprite*
  (load-sprite "images/player.png"
               #:position (vector2 (/ *window-width* 2)
				   (/ *window-height* 2))))

(define *player* (make-entity *player-sprite* (vector2 0 0)))

(define *bullet-texture* (load-texture "images/bullet.png"))

(define *bullets* (list))

(define *enemy-texture* (load-texture "images/enemy.png"))

(define *enemies* (list))

(define (shoot position)
  (set! *bullets*
	(append *bullets*
		(list (make-entity
		       (make-sprite *bullet-texture*
				    #:position position)
		       (vector2 0 4))))))

(define (quit-game)
  (close-window)
  (quit))

(define (move-by! sprite delta)
<<<<<<< HEAD
  (set-sprite-position! sprite (v+ (sprite-position sprite) delta)))
=======
  (set-sprite-position! sprite (vector2
				(+ (vx (sprite-position sprite))
				   (vx delta))
				(+ (vy (sprite-position sprite))
				   (vy delta)))))
>>>>>>> sprite-batch

(define (update-entity! entity)
  (move-by! (entity-sprite entity) (entity-velocity entity)))

(define (draw-entity entity)
  (draw-sprite (entity-sprite entity)))

(define (seconds->ticks seconds)
  (floor (* seconds 60)))

(define (move-left enemy)
  (schedule-action
   (lerp (lambda (x)
<<<<<<< HEAD
	   (set-entity-position! enemy (vector x (vy (entity-position enemy)))))
	 (- *window-width* 64) 64 (seconds->ticks 3))))
=======
	   (set-entity-position! enemy (vector2 x (vy (entity-position enemy)))))
	 (- *window-width* 64) 64 (seconds->timesteps 3))))
>>>>>>> sprite-batch

(define (spawn-enemy position)
  (let ((enemy (make-entity (make-sprite *enemy-texture*
					 #:position position)
			    (vector2 0 0))))
    (move-left enemy)
    (set! *enemies* (append *enemies* (list enemy)))))

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

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (quit-game))
	((any-equal? key 'a)
	 (set-entity-velocity! *player* (vector2 -1 0)))
	((any-equal? key 'd)
	 (set-entity-velocity! *player* (vector2 1 0)))
	((any-equal? key 's)
	 (set-entity-velocity! *player* (vector2 0 -1)))
	((any-equal? key 'w)
	 (set-entity-velocity! *player* (vector2 0 1)))
	((any-equal? key 'space)
	 (shoot (entity-position *player*)))
	((any-equal? key 'e)
	 (spawn-enemy (vector2 (- *window-width* 64)
			      (- *window-height* 64))))))

(define (update)
  (update-entity! *player*)
  (update-bullets! *bullets*))

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

;; Register hooks. Lambdas are used as "trampolines" so that render
;; and key-down can be redefined later and the hooks will call the
;; updated procedures.
(add-hook! on-quit-hook (lambda () (quit-game)))
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-update-hook (lambda () (update)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Start the game loop.
;; The render callback will be called through this procedure.
;(set! *show-fps* #f)
(run-game-loop)

