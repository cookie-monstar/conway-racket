#lang racket
(require 2htdp/universe 2htdp/image "conway.rkt")
(provide (all-defined-out))

(define live (rectangle 8 8 "solid" "black"))
(define dead (rectangle 8 8 "outline" "black"))

(define (list-draw x y lst)
  (foldr (lambda (a b) (overlay/xy a x y b)) (rectangle 0 0 100 "red") lst))
(define (grid-draw grid)
  (list-draw 0 8 (map (lambda (r) (list-draw 8 0 (map (lambda (c) (if (= c 1) live dead)) r))) grid)))

(define (animator grid)
  (lambda
    (n)
    (begin
      (set! grid (grid-next grid))
      (grid-draw grid))))