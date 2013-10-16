(define-module (square-man collision)
  #:use-module (srfi srfi-1)
  #:use-module (2d vector2)
  #:use-module (2d sprite)
  #:use-module (2d texture)
  #:use-module (square-man entity)
  #:export (make-collision-detection
	    make-collision-handler
	    make-collision-system
	    make-grid
	    make-collide?))

;;; A collision is a list of length two that containss the two colliding
;;; objects. (object1 object2)

(define (make-collision-detection narrow? broad)
  "Returns a new collision detector that takes a list of objects and returns a list of collisions. NARROW? is a function that returns
#t if two objects are actually colliding, and #f otherwise. Its takes a 
list of the two potentially colliding objects as its argument. BROAD is a 
function that takes a list of bjects and returns a list of all potential
collisions between them."
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

;;; A collision handler takes a list of collisions and reacts to them
(define* (make-collision-handler #:optional #:key
				(start (lambda (collisions) #f))
				(during (lambda (collisions) #f))
				(end (lambda (collisions) #f)))
  "Returns a new collision handler. START is a function that is called
when two objects first collide. DURING is a function that is called if two 
objects remain in contact after the initial collision. END is a
function that is called when two objects that were colliding stop colliding.
The handler functions (START DURING END) take a list of length two as their 
arguments, that contains the two colliding objects."
  (let ((previous-collisions (list)))
 
    (define (remove-previous-collision! collision)
      (set! previous-collisions (delete collision previous-collisions)))
    
    (define (handler collision)
      (if (or (member collision previous-collisions)
	      (member (reverse collision) previous-collisions))
	  (begin
	    (remove-previous-collision! collision)
	    (during collision))
	  (start collision)))
    
    (lambda (collisions)
      (for-each handler	collisions)
      (for-each end previous-collisions)
      (set! previous-collisions collisions))))

;;; Returns a function that when given state calls handler on all the objects that collide
;;; Objects is the function which when give state returns a list of all collidable object
;;; sprite-proc is the procedure that returns the sprite when given an entity
;;; broad is the broad-phase collision detection. this calls the spatial partioning to find possible collision.
;;; handler is the function that determines what happens when particular objects collide
(define (make-collision-system objects sprite-proc broad handler)
  "Returns a new collision system. It takes a state and responds to
all the collisions that occur in the state. OBJECTS is a function that
accepts a state and returns all the objects that can collide. SPRITE-PROC 
is a function that returns the sprite of an object in the state. BROAD is 
a function that takes a list of objects and returns a list of possible 
collision. HANDLER is a function that takes a list of collisions and 
responds to them."
  (define collide? (make-collide? sprite-proc))
  (define detect (make-collision-detection collide? broad))
  (lambda (state)
    (handler (delete-duplicates (detect (objects state))))))

;;; make a grid-based collision detection
(define (make-grid sprite-proc grid-height grid-width size)
  "Returns a new grid that takes a list of objects and returns 
all the possible collisions in it. SPRITE-PROC is a function that 
takes an object and returns its sprite. GRID-HEIGHT is the height in 
pixels of the grid. GRID-WIDTH is the width in pixels of the grid. 
SIZE is the size of a grid cell in pixels. "
  (define (make-empty-grid)
    (make-array '()
		(+ (ceiling (/ grid-height size)) 1)
		(+ (ceiling (/ grid-width size)) 1)))
  
  (define (position->gridtile position)
    (vector2 (ceiling (/ (vx position) size))
	     (ceiling (/ (vy position) size))))

  (define corner (make-corners sprite-proc))
  
  (define (corners entity)
    (list ((corner entity) 'top-right)
	  ((corner entity) 'top-left)
	  ((corner entity) 'bottom-right)
	  ((corner entity) 'bottom-left)))

  (let ((grid (make-empty-grid)))
   
    (define (cell-ref position)
      (array-ref grid (vy position) (vx position)))
    
    (define (set-cell! value position)
      (array-set! grid value (vy position) (vx position)))

    (define (insert! entity)
      (for-each (lambda (position)
		  (set-cell! (cons entity (cell-ref position)) position))
		(delete-duplicates
		 (map position->gridtile (corners entity)))))
    
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
  "Calls FUNCTION on all pairwise combinations of elements in LST. 
Returns a list of the results."
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
  "Calls FUNCTION on all pairwise combinations of elements in LST. 
Returns a list of the results, if a result is #f its not included 
in the list."
  (define (helper _lst accumulator)
    (if (null? _lst)
	accumulator
	(helper (cdr _lst)
		(append accumulator
			(filter-map (lambda (x)
			       (function (car _lst) x))
				    (cdr _lst))))))
  (helper lst (list)))

(define (make-corners sprite-proc)
  "Returns a function that takes an object and returns a corner of 
its sprite. Use the symbols 'bottom-right 'bottom-left 'top-right 
'top-left. SPRITE-PROC is a function that takes a object and returns 
its sprite."
  (lambda (entity)
    (let* ((sprite (sprite-proc entity))
	   (centre (sprite-position sprite))
	   (texture (sprite-drawable sprite))
	   (height (/ (texture-height texture) 2))
	   (width (/ (texture-width texture) 2)))
      (define (corner x y)
	(vector2 (x  (vx centre) width)
		 (y  (vy centre) height)))
      (lambda (arg)
	(case arg
	  ((bottom-right) (corner + +))
	  ((bottom-left) (corner - +))
	  ((top-right) (corner + -))
	  ((top-left) (corner - -)))))))

(define (make-collide? sprite-proc)
  "Returns a function that takes two objects an returns #t if they 
collide and #f otherwise. SPRITE-PROC is a function that takes an object 
and returns its sprite."
  (define corner (make-corners sprite-proc))
  (lambda (a b)
    (let ((top-left-a ((corner a) 'top-left))
	  (top-left-b ((corner b) 'top-left))
	  (bottom-right-a ((corner a) 'bottom-right))
	  (bottom-right-b ((corner b) 'bottom-right)))
      (not (or (< (vy bottom-right-a) (vy top-left-b))
	       (> (vy top-left-a) (vy bottom-right-b))
	       (< (vx bottom-right-a) (vx top-left-b))
	       (> (vx top-left-a) (vx bottom-right-b)))))))
