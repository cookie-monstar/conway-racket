#lang racket
(require 2htdp/universe 2htdp/image)
(provide (all-defined-out))

(define size 12)
(define live (rectangle size size "solid"   "black"))
(define dead (rectangle size size "outline" "black"))

(define (list-draw x y lst)
  (foldr (lambda (a b) (overlay/xy a x y b)) (rectangle 0 0 0 "red") lst))
(define (grid-draw grid)
  (list-draw 0 size (map (lambda (r) (list-draw size 0 (map (lambda (c) (if (= c 1) live dead)) r))) grid)))
