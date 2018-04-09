#lang racket
(require 2htdp/universe 2htdp/image "conway.rkt" "render.rkt" "parser.rkt" "list-comprehension.rkt")

;(define grid
;	(lc (lc (if (> (random) 0.6) 1 0) : x <- (range 32)) : y <- (range 256)))

(define nikhil (grid-expand (rle->grid "2o4bob3obo2bobobob3obo$obo3bo2bo2bobo2bobo2bo2bo$o2bo2bo2bo2b2o3b3o2bo2bo$o3bobo2bo2bobo2bobo2bo2bo$o4b2ob3obo2bobobob3ob4o$") 32 32))
(define grid (append (lc (make-list 32 0) : x <- (range 20)) nikhil (lc (make-list 32 0) : x <- (range 100))))

(define (tick-expr world)
	(set! grid (append (cddr (grid-next (cons (make-list 32 0) (take grid 12)))) (drop grid 12))))

(define (draw-expr world)
	(grid-draw (take grid 32)))

(define (stop-expr world) (= 32 (length grid)))

(big-bang (void)
	(name "Wow come welcome")
	(on-tick tick-expr 0.25)
	(to-draw draw-expr)
	(stop-when stop-expr))
