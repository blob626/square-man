(use-modules (2d sprite)
             (2d game-loop)
             (2d window)
	     (2d helpers)
	     (2d vector)
	     (2d texture)
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
  (load-sprite "/home/zan-xhipe/projects/square-man/player.png"
               #:position (vector (/ *window-width* 2)
                                  (/ *window-height* 2))))

(define *player* (make-entity *player-sprite* #(0 0)))

(define *bullet-texture* (load-texture "/home/zan-xhipe/projects/square-man/bullet.png"))

(define *bullets* (list))

(define (shoot position)
  (set! *bullets*
	(append *bullets*
		(list (make-entity
		       (make-sprite *bullet-texture*
				    #:position position)
		       #(0 4)))))
  (display *bullets*)
  (newline))

(define (quit-demo)
  (close-window)
  (quit))

(define (move-by! sprite delta)
  (set-sprite-position! sprite (vector
				(+ (vx (sprite-position sprite))
				   (vx delta))
				(+ (vy (sprite-position sprite))
				   (vy delta)))))

(define (update-entity! entity)
  (move-by! (entity-sprite entity) (entity-velocity entity)))

(define (draw-entity entity)
  (draw-sprite (entity-sprite entity)))

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

(define (draw-bullets bullets)
  (map draw-entity bullets))

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
	 (set-entity-velocity! *player* #(0 1)))
	((any-equal? key 'space)
	 (shoot (entity-position *player*)))))

;; Draw our sprite
(define (render)
  (draw-entity *player*)
  (update-entity! *player*)
  (update-bullets! *bullets*)
  (draw-bullets *bullets*))

;; Register hooks. Lambdas are used as "trampolines" so that render
;; and key-down can be redefined later and the hooks will call the
;; updated procedures.
(add-hook! on-quit-hook (lambda () (quit-demo)))
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)

