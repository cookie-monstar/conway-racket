#lang racket
(require 2htdp/universe "conway.rkt" "render.rkt" "parser.rkt" "hangar.rkt")

(define grid (grid-expand (rle->grid (car (dict-ref hangar "gosper glider gun"))) 64 64))

(define play-state #t)
(define file-state #f)

(define (tick-expr world)
	(if play-state (set! grid (grid-next grid)) (void))
		world)

(define (replace l n m)
	(list-set l n (m (list-ref l n))))

(define (mouse-expr world x y type)
	(if (mouse=? type "button-down") (set! grid (replace grid (quotient y size) (lambda (R) (replace R (quotient x size) (lambda (x) (- 1 x)))))) (void))
	world)

(define (key-expr world type)
	(cond
		[(key=? type "p") (set! play-state (not play-state))]
		[(key=? type "o") (set! file-state (not file-state))])
	world)

(define (draw-expr world)
	(grid-draw grid))

(define name-expr "Conway's Game of Life Simulator")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "Welcome to Simulator!\n")
(display "Instructions:\n")
(display " * Press p to toggle the simulation between play/pause\n")
(display " * Press o to import a patch from hanger\n")
(display " * Click a cell to toggle between live/dead\n")
(display "Enter \"Alelelele\" and hit enter to enter: ")
(define input (read))
(if (equal? input 'Alelelele)
	(begin
		(big-bang (void)
  		(on-tick tick-expr 0.25)
  		(on-mouse mouse-expr)
			(on-key key-expr)
  		(to-draw draw-expr)
			(name name-expr)))
	(begin
		(display "U typed \"")
		(display input)
		(display "\". Ur mom gay.\n")))
