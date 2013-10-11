(define-module (square-man collision)
  #:use-module (srfi srfi-1)
  #:use-module (2d vector2)
  #:export (make-collision-detection
	    make-collision-handler
	    make-collision-system
	    make-grid
	    square
	    distance-square))

(define (make-collision-detection narrow? broad)
  (define (narrow-phase possible-collisions)
    (pair-wise-filter-map
     (lambda (a b)
       (and (narrow? a b) (list a b)))
     possible-collisions))
  (define (value-or-#f value)
    (if (null? value)
	#f
	value))
  (lambda (objects)
    (apply append
	   (filter-map
	    (lambda (possible)
	      (value-or-#f (narrow-phase possible)))
	    (broad objects)))))

(define (make-collision-handler handler)
  (lambda (collisions)
    (for-each handler collisions)))

;;; objects is the function which when give state returns a list of all collidable objects
(define (make-collision-system objects broad narrow handler)
  (define detect (make-collision-detection narrow broad))
  (define handle (make-collision-handler handler))
  (lambda (state)
    (handle (detect (objects state)))))

(define (make-grid position-proc window-height window-width size)
  (define (make-empty-grid)
    (make-array '()
		(+ (ceiling (/ window-height size)) 1)
		(+ (ceiling (/ window-width size)) 1)))
  
  (define (position->gridtile position)
    (vector2 (ceiling (/ (vx position) size))
	     (ceiling (/ (vy position) size))))

  (let ((grid (make-empty-grid)))
   
    (define (cell-ref position)
      (array-ref grid (vy position) (vx position)))
    (define (set-cell! value position)
      (array-set! grid value (vy position) (vx position)))
    (define (insert! entity)
      (let ((position (position->gridtile (position-proc entity))))
	(set-cell! (cons entity (cell-ref position)) position)))
    (define (insert-all! lst)
      (clear-grid!)
      (for-each insert! lst))
    (define (clear-grid!)
      (set! grid (make-empty-grid)))
    (define (possible-collisions)
      (let ((possible '()))
	(array-for-each (lambda (cell)
			  (set! possible (cons cell possible)))
			grid)
	possible))
    (lambda (objects)
      (insert-all! objects)
      (possible-collisions))))

(define (pair-wise-map function lst)
  (define (helper _lst accumulator)
    (if (null? _lst)
	accumulator
	(helper (cdr _lst)
		(append accumulator
			(map (lambda (x)
			       (function (car _lst) x))
			     (cdr _lst))))))
  (helper lst (list)))

(define (pair-wise-filter-map function lst)
  (define (helper _lst accumulator)
    (if (null? _lst)
	accumulator
	(helper (cdr _lst)
		(append accumulator
			(filter-map (lambda (x)
			       (function (car _lst) x))
				    (cdr _lst))))))
  (helper lst (list)))

(define (square x)
  (* x x))

(define (distance-square a b)
  (+ (square (- (vx a) (vx b)))
     (square (- (vy a) (vy b)))))
