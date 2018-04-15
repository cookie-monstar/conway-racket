#lang racket
(provide (all-defined-out))
(define (grid-god l c u)
  (lambda (g n)
    (cond [(> l n) 0]
          [(= c n) 1]
          [(< u n) 0]
          [else g])))
(define (grid-add A B)
  (map (lambda (a b) (map (lambda (x y) (+ x y)) a b)) A B))
(define conway-god (grid-god 2 3 3))
(define (grid-n grid)
  (append (cdr grid) (list (car grid))))
(define (grid-s grid)
  (cons (last grid) (take grid (- (length grid) 1))))
(define (grid-w grid)
  (map grid-n grid))
(define (grid-e grid)
  (map grid-s grid))
(define (get-neighbours grid)
  (define lst (list grid))
  (begin
    (set! lst (append* (map (lambda (g) (list g (grid-w g) (grid-e g))) lst)))
    (set! lst (append* (map (lambda (g) (list g (grid-n g) (grid-s g))) lst)))
    (foldr grid-add (cadr lst) (cddr lst))))
(define (grid-next grid)
  (map
    (lambda (g n) (map conway-god g n))
    grid
    (get-neighbours grid)))
