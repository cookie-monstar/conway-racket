#lang racket
(require 2htdp/universe 2htdp/image)
(provide (all-defined-out))
(require "list-comprehension.rkt")

(define size 12)
(define live (rectangle size size "solid"   "black"))
(define dead (rectangle size size "outline" "black"))

(define (grid-draw grid)
	(apply above (lc (apply beside (lc (if (= 0 cell) dead live) : cell <- row)) : row <- grid)))
