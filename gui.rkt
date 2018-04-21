#lang racket
(require racket/gui (prefix-in htdp: 2htdp/image) "parser.rkt" "hangar.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;code credited to GoZoner, https://stackoverflow.com/a/16493261/3086544
(define-syntax try
  (syntax-rules (catch)
    ((_ body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
        (with-exception-handler
         (lambda (condition)
           catcher
           (exit condition))
         (lambda () body)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;parsing capabilities
(define (rle->grid rle)
  (define x 0)
  (reverse (map reverse (foldl
                         (lambda (c l)
                           (cond
                             [(= 98 c) (cons (if
                                              (> x 0)
                                              (append (make-list (begin0 x (set! x 0)) 0) (car l))
                                              (cons 0 (car l))) (cdr l))]
                             [(= 111 c) (cons (if
                                               (> x 0)
                                               (append (make-list (begin0 x (set! x 0)) 1) (car l))
                                               (cons 1 (car l))) (cdr l))]
                             [(= 36 c) (if (> x 0)
                                           (append (make-list (begin0 x (set! x 0)) '()) l)
                                           (cons '() l))]
                             [(and (> c 47) (< c 58)) (begin (set! x (+ (- c 48) (* 10 x))) l)]
                             [else l]))
                         '(())
                         (map char->integer (string->list rle))))))
(define (get-rle path)
  (define in (open-input-file (string-append "hangar/" path ".rle")))
  (define grid "")
  (define (reader)
    (define line (read-line in))
    (cond
      [(eq? eof line) (rle->grid grid)]
      [(eq? #\# (string-ref line 0)) (reader)]
      [(eq? #\x (string-ref line 0)) (reader)]
      [else (begin (set! grid (string-append grid line)) (reader))]))
  (set! grid (reader))
  (grid-expand grid (apply max (map length grid)) (length grid)))
(define (list-expand bait n)
  (lambda (lst) (append lst (make-list (- n (length lst)) bait))))
(define (grid-expand grid m n)
  (begin
    (set! grid (map (list-expand 0 m) grid))
    (set! grid ((list-expand (make-list m 0) n) grid))
    grid))
(define grid->rle ((lambda ()
	(define (list->rle lst)
		(define n 1)
		(define l (car lst))
		(foldl (lambda (str lit)
			(if
				(eq? l lit)
				(begin
					(set! n (+ n 1))
					str)
				(begin0
					(string-append str (if (= 1 n) "" (number->string n)) (if (= 0 l) "b" "o"))
					(set! n 1)
					(set! l lit))
				))
			""
			(cdr lst)))
	(lambda (grid)
	(foldr string-append "" (map list->rle grid) (make-list (- (length grid) 1) "$"))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;grid characteristics and algorithms
(define (grid-god l c u)
  (lambda (g n)
    (cond [(> l n) 0]
          [(= c n) 1]
          [(< u n) 0]
          [else g])))
(define (grid-add A B)
  (map (lambda (a b) (map (lambda (x y) (+ x y)) a b)) A B))
(define conway-god (grid-god 2 3 3))
(define (grid-or A B)
  (map (lambda (a b) (map (lambda (x y) (cond ((and (= x 1) (= y 1)) 1)
                                              ((or (and (= x 1) (= y 1)) (and (= x 0) (= y 0))) 1)
                                              ((and (= x 0) (= y 0)) 0)))
                          a b)) A B))
(define (grid-n grid)
  (append (cdr grid) (list (car grid))))
(define (grid-s grid)
  (cons (last grid) (take grid (- (length grid) 1))))
(define (grid-w grid)
  (map grid-n grid))
(define (grid-e grid)
  (map grid-s grid))
(define (grid-move grid x y)
  (set! grid (append (drop grid (- (length grid) y)) (take grid (- (length grid) y))))
  (map (lambda (row) (append (drop row (- (length row) x)) (take row (- (length row) x)))) grid))
(define grid-type 0)
(define (get-toroid-neighbours grid)
  (define lst (list grid))
  (begin
    (set! lst (append* (map (lambda (g) (list g (grid-w g) (grid-e g))) lst)))
    (set! lst (append* (map (lambda (g) (list g (grid-n g) (grid-s g))) lst)))
    (foldr grid-add (cadr lst) (cddr lst))))

(define (get-neighbours grid)
  (cond
    [(eq? grid-type 0) (get-toroid-neighbours grid)]
    [(eq? grid-type 1) (map cdr (cdr (get-toroid-neighbours (cons (make-list (+ 1 (length (car grid))) 0) (map (lambda (x) (cons 0 x)) grid)))))]))

(define (grid-next grid)
  (map
   (lambda (g n) (map conway-god g n))
   grid
   (get-neighbours grid)))
(define (grid-jump grid x)
  (void (map (lambda (x) (set! grid (grid-next grid))) (range x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;welcome frame
(define welcome-frame
  (new frame%
       [label "Conway's game of life"]
       [width 512]
       [height 512]))
(define welcome-panel
  (new vertical-panel%
       [parent welcome-frame]
       [alignment '(center center)]))
(define new-game
  (new button%
       [parent welcome-panel]
       [label "New game"]))
(define load-game
  (new button%
       [parent welcome-panel]
       [label "Load game"]))
(send welcome-frame show #t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cell-size 4)
(define grid-size '(192 . 192))
(define frame
  (new frame%
       [label "Conway's game of life"]
       [width 1024]
       [height 768]))
(define panels
  (new horizontal-panel%
       [parent frame]))
(define panel-left
  (new panel%
       [parent panels]
       [border 1]
       [min-width 768]
       [min-height 768]))
(define panel-right
  (new vertical-panel%
       [parent panels]
       [min-width 256]
       [min-height 768]))
(define play-state #f)
(define patch-state #f)
(define play-button
  (new button%
       [parent panel-right]
       [label "Paused"]
       [callback (lambda (button event) (set! play-state (not play-state))
                   (send play-button set-label
                         (if play-state
                             (begin
                               (send timer start speed-time)
                               "Playing")
                             (begin
                               (send timer stop)
                               "Paused"))))]))
(define next-button
  (new button%
       [parent panel-right]
       [label "Next"]
       [callback (lambda (button event) (canvas-swap (send canvas get-dc) (grid-next grid)))]))
(define jump-field
  (new text-field%
       [parent panel-right]
       [label "Jump by: "]))
(define jump-button
  (new button%
       [parent panel-right]
       [label "Jump"]
       [callback (lambda (button event)
                   (define x (send jump-field get-value))
                   (display x)
                   )]))
(define grid-choice
  (new choice%
       [parent panel-right]
       [label "Grid type: "]
       [choices (list "Closed" "Open")]
       [callback (lambda (choice type) (set! grid-type (send choice get-selection)))]))
(define speed-time 100)
(define speed-choice
  (new choice%
       [parent panel-right]
       [label "Speed: "]
       [choices (list "Slow" "Normal" "Fast")]
       [selection 1]
       [callback (lambda (choice type) (set! speed-time (list-ref '(500 100 20) (send choice get-selection))) (when play-state (send timer stop) (send timer start speed-time)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define live (make-object brush% "BLACK" 'solid))
(define dead (make-object brush% "WHITE" 'solid))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (canvas-draw dc grid-new)
  (set! grid grid-new)
  (map (lambda (row)
         (map (lambda (col)
                (send dc set-brush (if (= 1 (list-ref (list-ref grid row) col)) live dead))
                (send dc draw-rectangle (* cell-size col) (* cell-size row) cell-size cell-size))
              (range (car grid-size))))
       (range (cdr grid-size))))
(define (canvas-swap dc grid-new)
  (map (lambda (row)
         (map (lambda (col)
                (cond
                  [(= (list-ref (list-ref grid row) col)(list-ref (list-ref grid-new row) col)) (void)]
                  [else (begin
                          (send dc set-brush (if (= 1 (list-ref (list-ref grid-new row) col)) live dead))
                          (send dc draw-rectangle (* cell-size col) (* cell-size row) cell-size cell-size))]))
              (range (car grid-size))))
       (range (cdr grid-size)))
  (set! grid grid-new))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define grid (grid-expand (get-rle "Guns/vacuum-cleaner") (car grid-size) (cdr grid-size)))
;(define grid (grid-move
;              (grid-expand (rle->grid (car (dict-ref hangar "puffer train"))) (car grid-size) (cdr grid-size)) (/ (car grid-size) 2) (/ (cdr grid-size) 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define timer #f)
(define canvas
  (new
   (class canvas%
     (define/override (on-event e)
       (when (eq? (send e get-event-type) 'left-down)
         (define x (quotient (send e get-x) cell-size))
         (define y (quotient (send e get-y) cell-size))
         (canvas-swap (send canvas get-dc)
                      (list-set grid y
                                (list-set (list-ref grid y) x
                                          (- 1 (list-ref (list-ref grid y) x)))))))
     (super-new))
   [parent panel-left]
   [min-width 768]
   [min-height 768]
   [paint-callback
    (lambda (canvas dc)
      (canvas-draw dc grid)
      (set! timer
            (new timer%
                 [notify-callback (lambda ()
                                    (if (send frame is-shown?) (canvas-swap dc (grid-next grid)) (send timer stop)))])))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;