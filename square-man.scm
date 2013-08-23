(use-modules (2d sprite)
             (2d game-loop)
             (2d window)
	     (2d helpers))

(define *window-width* 800)
(define *window-height* 600)

;; Open the window.
(open-window *window-width* *window-height*)

;; Load a sprite and center it on the screen.
(define sprite
  (load-sprite "/home/zan-xhipe/projects/square-man/sprite.png"
               #:position (vector (/ *window-width* 2)
                                  (/ *window-height* 2))))

(define (quit-demo)
  (close-window)
  (quit))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (quit-demo))))

;; Draw our sprite
(define (render)
  (draw-sprite sprite))

;; Register hooks. Lambdas are used as "trampolines" so that render
;; and key-down can be redefined later and the hooks will call the
;; updated procedures.
(add-hook! on-quit-hook (lambda () (quit-demo)))
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)

