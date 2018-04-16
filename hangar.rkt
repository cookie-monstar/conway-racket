#lang racket
(provide (all-defined-out))
(require "parser.rkt" "render.rkt")
(define hangar (list
	(list "gosper glider gun"
		"24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o" 64 64)
	(list "pulsar"
		"2b3o3b3o2b2$o4bobo4bo$o4bobo4bo$o4bobo4bo$2b3o3b3o2b2$2b3o3b3o2b$o4bobo4bo$o4bobo4bo$o4bobo4bo2$2b3o3b3o" 13 13)
	(list "beacon"
		"2o2b$o3b$3bo$2b2o" 4 4)
	(list "pentadecathlon"
		"2bo4bo2b$2ob4ob2o$2bo4bo" 10 3)
	(list "toad"
		"b3o$3" 4 2)
	(list "blinker"
		"3o" 3 1)
	(list "glider"
		"bob$2bo$3o" 3 3)
	(list "light weight spaceship"
		"bo2bo$o4b$o3bo$4o" 5 4)
	(list "block"
		"2o$2o" 2 2)
	(list "beehive"
		"b2ob$o2bo$b2o" 4 3)
	(list "loaf"
		"b2ob$o2bo$bobo$2bo" 4 4)
	(list "replicator"
		"2b3o$bo2bo$o3bo$o2bob$3o" 5 5)
	(list "aircraft carrier"
		"2o2b$o2bo$2b2o" 4 3)))
