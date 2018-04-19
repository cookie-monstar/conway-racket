#lang racket
(require 2htdp/universe 2htdp/image "conway.rkt" "render.rkt" "parser.rkt" "hangar.rkt")

(define grid (grid-expand (rle->grid (car (dict-ref hangar "gosper glider gun"))) 64 64))

(define play-state #t)
(define (tick-expr world)
	(if play-state (set! grid (grid-next grid)) (void))
		world)
(define (replace l n m)
	(list-set l n (m (list-ref l n))))

(define bool #f) 
(define (mouse-expr world x y type)
  (set! coords (list x y))
  (if bool
      (begin (set! bool #f)
         (if (mouse=? type "button-down")
             (grid-or
              (grid-move (grid-expand (grid-draw patch) 64 64) (quotient (car coords) size) (quotient (cadr coords) size))
              grid)
            (void)))

	(if (mouse=? type "button-down") (set! grid (replace grid (quotient y size)
                                                             (lambda (R) (replace R (quotient x size) (lambda (x) (- 1 x))))))
            (void)))
	world)
(define (show-hangar)
	(void (map (lambda (x) (display x) (display ".") (displayln (car (list-ref hangar x)))) (range (length hangar)))))

(define patch #f)
(define coords '(0 0))

(define (key-expr world type)
	(cond
		[(key=? type "p") (set! play-state (not play-state))]
		[(key=? type "c") (set! grid (grid-expand '() (length (car grid)) (length grid)))]
		[(key=? type "l") (show-hangar)]
		[(key=? type "o") (begin
                                    (set! bool #t)
                                    (define input (read))
                                    (if (and (>= input 0) (< input (length hangar)))
                                      (get-hangar input) (void)))])
	world)
(define (get-hangar input)
  (define inp (cdr (list-ref hangar input)))
  (set! patch (grid-expand (rle->grid (car inp)) (caddr inp) (cadr inp))))



(define (draw-expr world)
  (if patch
      (overlay/xy (grid-draw grid) (car coords) (cadr coords) (grid-draw patch) )
      (grid-draw grid)))
(define name-expr "Conway's Game of Life Simulator")
(display "Welcome to Simulator!\n")
(display "Instructions:\n")
(display " * Press P to toggle the simulation between play/pause\n")
(display " * Press O and a number to import a patch from hangar\n")
(display " * Press L to list out the hanger patches")
(display " * Click a cell to toggle between live/dead\n")
(display "Enter \"Alelelele\" and hit enter to enter: ")
(define input 'Alelelele);(read))
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
