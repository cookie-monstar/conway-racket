#lang racket
(require 2htdp/universe "conway.rkt" "gui.rkt")

(define world
  '(#t
    (0 1 0 0 0 0 0 0 0 0 0 0)
    (0 0 1 0 0 0 0 0 0 0 0 0)
    (1 1 1 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0)))

(define (grid-tick world)
	(cons (car world) (if (car world) (grid-next (cdr world)) (cdr world))))

(define (replace l n m)
	(list-set l n (m (list-ref l n))))

(define (operation world x y type)
	(if (mouse=? type "button-down")
	(cons (car world) (replace (cdr world) (quotient y 8) (lambda (R) (replace R (quotient x 8) (lambda (x) (- 1 x))))))
	world))

(define (key-expr world type)
	(if (key=? type "p")
	(list-set world 0 (not (car world)))
	world))

(define (world-draw world)
	(grid-draw (cdr world)))

(big-bang
    world
  (on-tick grid-tick 1)
  (on-mouse operation)
  (on-key key-expr)
  (to-draw world-draw))
