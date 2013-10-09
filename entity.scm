(define-module (square-man entity)
  #:use-module (srfi srfi-9)
  #:use-module (2d vector2)
  #:use-module (2d sprite)
  #:export (<entity>
	    make-entity
	    entity?

	    update-entity!
	    draw-entity
	    
	    entity-sprite
	    set-entity-sprite!
	    entity-velocity
	    set-entity-velocity!
	    entity-position
	    set-entity-position!))


(define-record-type <entity>
  (make-entity sprite velocity)
  entity?
  (sprite entity-sprite set-entity-sprite!)
  (velocity entity-velocity set-entity-velocity!))

(define (entity-position entity)
  (sprite-position (entity-sprite entity)))

(define (set-entity-position! entity value)
  (set-sprite-position! (entity-sprite entity) value))

(define (update-entity! entity)
  (set-entity-position! entity (v+
				(entity-position entity)
				(entity-velocity entity))))

(define (draw-entity entity)
  (draw-sprite (entity-sprite entity)))
