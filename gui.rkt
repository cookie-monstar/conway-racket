#lang racket
(require racket/gui (prefix-in htdp: 2htdp/image) "conway.rkt" "parser.rkt" "hangar.rkt")
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
  (new panel%
       [parent panels]
       [min-width 256]
       [min-height 768]))
(define play-state #f)
(define play-button
  (new button%
       [parent panel-right]
       [label "Paused"]
       [callback (lambda (button event) (set! play-state (not play-state)) (send play-button set-label (if play-state "Playing" "Paused")))]))
(define grid-select
  (new 
(define timer #f)
(define canvas
  (new canvas%
       [parent panel-left]
       ;[style '(gl)]
       ;[border 1]
       [min-width 768]
       [min-height 768]
       [paint-callback (lambda (canvas dc)
                         (set! timer
                           (new timer%
                                [notify-callback (lambda () (draw-canvas canvas dc))]
                                [interval 100])))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define live (make-object brush% "BLACK" 'solid))
(define dead (make-object brush% "WHITE" 'solid))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-canvas canvas dc)
  (map (lambda (row)
         (map (lambda (col)
                (send dc set-brush (if (= 1 (list-ref (list-ref grid row) col)) live dead))
                (send dc draw-rectangle (* 12 col) (* 12 row) 12 12))
              (range (length (car grid)))))
       (range (length grid)))
	   (if play-state (set! grid (grid-next grid)) (void)))
(define grid (grid-expand (rle->grid (car (dict-ref hangar "gosper glider gun"))) 64 64))
