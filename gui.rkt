#lang racket
(require 2htdp/image 2htdp/universe "conway.rkt")
(provide (all-defined-out))

(define live (rectangle 16 16 "solid" "green"))
(define dead (rectangle 16 16 "solid" "black"))

(define (list-draw x y lst)
	(foldr (lambda (a b) (overlay/xy a x y b)) (rectangle 0 0 100 "red") lst))
(define (grid-draw grid)
	(list-draw 0 16 (map (lambda (r) (list-draw 16 0 (map (lambda (c) (if (= c 1) live dead)) r))) grid)))

(define grid
	'((0 1 0 0 0 0 0 0 0 0 0 0)
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

(define (run n)
	(begin (set! grid (grid-next grid)) (grid-draw grid)))

(animate run)
