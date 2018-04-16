#lang racket
(require 2htdp/universe 2htdp/image "conway.rkt" "render.rkt" "parser.rkt" "list-comprehension.rkt" "thanku.rkt")

;(define grid
;	(lc (lc (if (> (random) 0.6) 1 0) : x <- (range 32)) : y <- (range 256)))

(define nikhil (grid-expand (rle->grid
                             "2o4bob3obo2bobobob3obo$obo3bo2bo2bobo2bobo2bo2bo$o2bo2bo2bo2b2o3b3o2bo2bo$o3bobo2bo2bobo2bobo2bo2bo$o4b2ob3obo2bobobob3ob4o$") 32 8))
(define poorvi (grid-expand (rle->grid "3o3b2o3b2o2b3o2bo5bob3o$o2bobo2bobo2bobo2bobo5bo2bo$3o2bo2bobo2bob3o3bo3bo3bo$o4bo2bobo2bobobo4bobo4bo$o5b2o3b2o2bo2bo4bo4b3o$") 32 8))
(define grid (append (lc (make-list 32 0) : x <- (range 10)) nikhil poorvi (lc (make-list 32 0) : x <- (range 100))))

(define (tick-expr world)
  (begin
	(set! grid (append (take
                            (cddr (grid-next
                                   (cons (make-list 32 0) (take grid 13))))
                            11)
                           (drop grid 12)))))

(define (draw-expr world)
	(grid-draw (take grid 32)))
(define count 0)
(define (tick-expression world)
  (cond ((< count 32) (begin
                         (set! count (+ count 1))
                         (set! grid (append (cdr (grid-next (cons (make-list 32 0) (take grid count))))
                                            (drop grid count)))))
        ((< count 40) (begin
                         (set! count (+ count 1))(set! grid (grid-next grid))))
        ((< count 45) (begin
                         (set! count (+ count 1))(set! grid thanku)))
        ((< count 55) (begin
                         (set! count (+ count 1))(set! grid (grid-next grid))))))
  

(define (stop-expr world) (= 120 count))

(big-bang (void) 
	(name "Wow come welcome")
	(on-tick tick-expression 0.25)
	(to-draw draw-expr)
	(stop-when stop-expr))

(define (takingfunc l n1 n2)
  (drop (take l n2) (- n1 1)))
