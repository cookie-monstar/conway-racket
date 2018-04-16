#lang racket
(provide (all-defined-out))
(require "parser.rkt")
(require "render.rkt")

(define thanku (grid-expand (rle->grid "$$$$$$$$$$$b5obo3bo2b2o2b2o4bobo2bo$
3bo3bo3bobo2bobobo3bobobo$
3bo3b5ob4obo2bo2bob2o$
3bo3bo3bobo2bobo3bobobobo$
3bo3bo3bobo2bobo4b2obo2bo$
$
$
10bo3bo2b2o2bo3bo$
11bobo2bo2bobo3bo$
12bo3bo2bobo3bo$
12bo3bo2bobo3bo$
12bo4b2o3b3o$") 32 32))

