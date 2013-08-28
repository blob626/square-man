;; load the SDL module and some useful srfi's
(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (2d game-loop)
             (2d helpers)
             (2d sprite)
             (2d texture)
             (2d vector2)
             (2d window))

(set! *random-state* (random-state-from-platform))

;;;
;;; Particles
;;;

(define-record-type <particle>
  (make-particle sprite position velocity)
  particle?
  (sprite particle-sprite)
  (position particle-position set-particle-position!)
  (velocity particle-velocity set-particle-velocity!))

(define (update-particle! particle)
  (set-particle-position! particle
                          (v+ (particle-position particle)
                              (particle-velocity particle))))

;;;
;;; Demo
;;;

(define window-width 800)
(define window-height 600)

(open-window window-width window-height)

(define stars (load-sprite "images/stars.png" #:anchor null-vector2))
(define particle-image (load-texture "images/bullet.png"))
(define particle-width (texture-width particle-image))
(define particle-height (texture-height particle-image))
(define particle-count 50)
(define particles
  (list-tabulate particle-count
                 (lambda (n)
                   (make-particle (make-sprite particle-image)
                                  (vector2 (random window-width)
                                           (random window-height))
                                  (vector2 (* (random:normal) 1)
                                           (* (random:normal) 1))))))
(define batch (make-sprite-batch (* particle-count 4)))

(define *player-image* (load-texture "images/player.png"))
(define *player-particle* (make-particle (make-sprite *player-image*)
					 (vector2 0 0)
					 (vector2 0 0)))

(define (draw-particles particles)
  (with-sprite-batch batch
    (for-each
     (lambda (p)
       (let* ((sprite (particle-sprite p)))
         (set-sprite-position! sprite (particle-position p))
         (draw-sprite sprite)))
     (append particles (list *player-particle*)))))

(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-update-hook (lambda () (update)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (close-window)
         (quit))))

(define (render)
  (draw-sprite stars)
  (draw-particles particles))

(define (update)
  (for-each update-particle! particles))

(run-game-loop)
