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
		"2o2b$o2bo$2b2o" 4 3)
        (list "knightship" "4b2o$4bo2bo$4bo3bo$6b3o$2b2o6b4o$2bob2o4b4o$bo4bo6b3o$2b4o4b2o3bo$o9b
2o$bo3bo$6b3o2b2o2bo$2b2o7bo4bo$13bob2o$10b2o6bo$11b2ob3obo$10b2o3bo2b
o$10bobo2b2o$10bo2bobobo$10b3o6bo$11bobobo3bo$14b2obobo$11bo6b3o2$11bo
9bo$11bo3bo6bo$12bo5b5o$12b3o$16b2o$13b3o2bo$11bob3obo$10bo3bo2bo$11bo
4b2ob3o$13b4obo4b2o$13bob4o4b2o$19bo$20bo2b2o$20b2o$21b5o$25b2o$19b3o
6bo$20bobo3bobo$19bo3bo3bo$19bo3b2o$18bo6bob3o$19b2o3bo3b2o$20b4o2bo2b
o$22b2o3bo$21bo$21b2obo$20bo$19b5o$19bo4bo$18b3ob3o$18bob5o$18bo$20bo$
16bo4b4o$20b4ob2o$17b3o4bo$24bobo$28bo$24bo2b2o$25b3o$22b2o$21b3o5bo$
24b2o2bobo$21bo2b3obobo$22b2obo2bo$24bobo2b2o$26b2o$22b3o4bo$22b3o4bo$
23b2o3b3o$24b2ob2o$25b2o$25bo2$24b2o$26bo" 31 79)
        (list "puffer train"
              "3bo$4bo$o3bo$b4o4$o$boo$bbo$bbo$bo3$3bo$4bo$o3bo$b4o" 5 18)))