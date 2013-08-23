(use-modules (2d sprite)
             (2d game-loop)
             (2d window)
	     (2d helpers)
	     (2d vector)

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
  (sprite-position entity))

(define (set-entity-position! entity value)
  (set-sprite-position! entity value))

;; Load a sprite and center it on the screen.
(define *sprite*
  (load-sprite "/home/zan-xhipe/projects/square-man/player.png"
               #:position (vector (/ *window-width* 2)
                                  (/ *window-height* 2))))

(define *player* (make-entity *sprite* #(0 0)))

(define (quit-demo)
  (close-window)
  (quit))

(define (move-by sprite delta)
  (set-sprite-position! sprite (vector
				(+ (vx (sprite-position sprite))
				   (vx delta))
				(+ (vy (sprite-position sprite))
				   (vy delta)))))

(define (update-enity entity)
  (move-by (entity-sprite entity) (entity-velocity entity)))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (quit-demo))
	((any-equal? key 'a)
	 (set-entity-velocity! *player* #(-1 0)))
	((any-equal? key 'd)
	 (set-entity-velocity! *player* #(1 0)))
	((any-equal? key 's)
	 (set-entity-velocity! *player* #(0 -1)))
	((any-equal? key 'w)
	 (set-entity-velocity! *player* #(0 1)))))

;; Draw our sprite
(define (render)
  (draw-sprite *sprite*)
  (update-enity *player*))

;; Register hooks. Lambdas are used as "trampolines" so that render
;; and key-down can be redefined later and the hooks will call the
;; updated procedures.
(add-hook! on-quit-hook (lambda () (quit-demo)))
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)

