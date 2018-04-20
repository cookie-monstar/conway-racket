#lang racket
(require racket/gui (prefix-in htdp: 2htdp/image) "parser.rkt" "hangar.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cell-size 4)
(define grid-size '(192 . 192))
(define frame
  (new frame%
       [label "Conway's game of life"]
       [width 1024]
       [height 768]))
(send frame show #t)
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
                               (send timer start 1)
                               "Playing")
                             (begin
                               (send timer stop)
                               "Paused"))))]))
(define grid-button
  (new choice%
       [parent panel-right]
       [label "Grid type: "]
       [choices (list "Closed" "Open")]
       [callback (lambda (choice type) (set! grid-type (send choice get-selection)))]))
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
(define grid (grid-move
              (grid-expand (rle->grid (car (dict-ref hangar "knightship"))) (car grid-size) (cdr grid-size)) (/ (car grid-size) 2) (/ (cdr grid-size) 2)))
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
                                    (if (send frame is-shown?) (canvas-swap dc (grid-next grid)) (send timer stop)))]
                 [interval 50]))
      (send timer stop))]))
