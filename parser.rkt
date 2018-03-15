#lang racket
(provide (all-defined-out))

(define (rle->string rle)
	(define x 0)
	(define t 0)
	(reverse (map reverse (foldl
		(lambda (c l)
			(cond
				[(= 98 c) (cons (if
													(> x 0)
													(append (make-list (begin (set! t x) (set! x 0) t) 0) (car l))
													(cons 0 (car l))) (cdr l))]
				[(= 111 c) (cons (if
													(> x 0)
													(append (make-list (begin (set! t x) (set! x 0) t) 1) (car l))
													(cons 1 (car l))) (cdr l))]
				[(= 36 c) (cons '() l)]
				[(and (> c 47) (< c 58)) (begin (set! x (+ (- c 48) (* 10 x))) l)]
				[else l]))
		'(())
		(map char->integer (string->list rle))))))

(define (string->grid str)
	(foldr
		(lambda
			(x y)
			(cond [(eq? x #\.) (cons '() y)]
						[(eq? x #\1) (cons (cons 1 (car y)) (cdr y))]
						[(eq? x #\0) (cons (cons 0 (car y)) (cdr y))]
						[else y]))
		'(())
		(string->list str)))

(define (list-expander bait n)
	(lambda (lst) (append lst (make-list (- n (length lst)) bait))))

(define (grid-expand grid m n)
	(begin
		(set! grid (map (list-expander 0 m) grid))
		(set! grid ((list-expander (make-list m 0) n) grid))
		grid))
