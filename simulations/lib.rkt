#lang racket
(require 2htdp/universe "../gui.rkt" "../parser.rkt")
(provide (all-defined-out))

(define (simulate str x y)
(animate (animator (grid-expand (rle->string
str
) x y))))
