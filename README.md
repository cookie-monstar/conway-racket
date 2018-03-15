# conway-racket
A racket implementation of conway's game of life and applying it to have some fun :)
##Directory Tree
* **conway.rkt** - Library for computations on grids using the rules
* **gui.rkt** - Graphical rendering and animating
* **parser.rkt** - Basic parser to conver RLE into grids
* **simulations/** - Many simulations to play with
	* **lib.rkt** - Library to make simulations
	* **stables/** - All pattern which remian the same
	*	**oscillators/** - All patterns which oscillate with finite cycle length
	* **vehicles/** - All patters which traverse through the grid
	* **explosions/** - All patterns which go haywired and span a large area after completion
##Things to do
* Use [HashLife algorithm](https://en.wikipedia.org/wiki/HashLife) to make better and faster simulations
* Multiple topological options for grids
* Function for efficiently translating the pattern on grid to other places
* Enriched simlulator experience
* Insitu grid editing options
* `grid->rle` converter?
* Football and Credits still remain a mystery
