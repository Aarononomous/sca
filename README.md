# Modeling Worlds with Stochastic Cellular Automata

## Overview

> We may regard the present state of the universe as the effect of its past and the cause of its future.  
	&mdash; Pierre-Simon Laplace, *Essai philosophique sur les probabilités*, 1814

We can create and explore sophisticated models of the world with one simple mechanism: within the model, a grid of cells, updated at discrete intervals, the future state of each cell is determined solely by its current state and the current states of its neighbors.

This model is called __cellular automata__. A noteworthy example of this is the Game of Life, invented by John Conway in 1970. From only four rules, powerful and surprising behavior emerges. Even without direct interaction between entities, group organization seems to be present.

The world is haphazard, and by introducing randomness to the state transitions (the “stochastic” part of “stochastic cellular automata”), we add additional subtlety and complexity to our simulations.


This project consists of:

1. a simulator for running and exploring these models;
2. a language that describes models; and
3. a bunch of fun models to play with.

## More

A really wonderful explication of this can be seen (and played with) at [Simulating the World (in Emoji)](http://ncase.me/simulating), and I urge everybody to do so.

To introduce stochastic cellular automata, let’s jump in and  construct a simple model. The [predator-prey equations](https://en.wikipedia.org/wiki/Lotka–Volterra_equations) describe cyclical changes in population in two species of animals, predators and prey --- the cyclic nature develops from the delay between prey population and predation. It can be described with differential equations, but we’ll create another model of this relationship via a different mechanism, cellular automata. There are three states in the model: empty, wolf (🐺), and bunny (🐰).

Next, let's devise transition rules for each of these states.

__Empty cells:__

* if more than two neighbors are bunnies (50% of the time) ⟶ become a bunny; __reproduction__  
	`(trans 'empty (* (neighbor> 2 '🐰) 0.5) '🐰)`
* if more than two neighbors are wolves (25% of the time) ⟶ become a wolf; __reproduction__  
	`(trans 'empty (* (neighbor> 2 '🐺) 0.25) '🐺)`

__Bunny cells:__

* if one neighbor is a wolf ⟶ become empty; __predation__  
	`(trans '🐰 (neighbor= 1 '🐺) 'empty)`
* if more than one neighbor is a wolf ⟶ become a wolf; __predation and reproduction__  
	`(trans '🐰 (neighbor> 1 '🐺) '🐺)`
* if more than three neighbors are bunnies ⟶ become empty; __overconsumption__  
	`(trans '🐰 (neighbor> 3 '🐰) 'empty)`

__Wolf cells:__

* if less than one of its neighbors is a bunny ⟶ become empty; __starvation__  
	`(trans '🐺 (neighbor< 1 '🐰) 'empty)`

And that's it!

Note that the automata cells are similar, but *not exactly* equivalent to agents. A better metaphor would be to think of them as locations displaying the plurality of their inhabitants.

We then set up the "world" with randomly populated cells, say 1/3 bunny and 1/10 wolf, and begin the simulation. After ten or so generations, patterns begin emerging. Small groups of wolves congregate over five generations, deplete all the bunnies in the surrounding region, and then break off into  “packs” which rove across the world leaving wakes of cleared cells, which then repopulate with bunnies in another dozen generations. From the first random state, then, cyclic populations emerge, while epicyclic waves of population density come and go.

## Implementation

### A Slow Start

You'll need a few things not included in this repo to get started, unfortunately.

* [SBCL](http://www.sbcl.org) is what I'm using, but any good Common Lisp should work.
* [Quicklisp](https://www.quicklisp.org/) is a library manager for Lisp, used to install
* [cl-charms](https://github.com/HiTECNOLOGYs/cl-charms), a curses screen manager. This will be installed automatically the first time the program's run, but Quicklisp has to be installed separately.

### The Simulator

The terminal window is used as the display. With the exception of an activity bar which occupies its bottom line, the entire terminal is occupied by the simulation. At every generation, all cells within the model are updated and the world is re-displayed.

Controls are simple: a list of available options is listed on the activity bar, and are activated by their key:

__[P]ause/Play__: Does the obvious thing.

__[L]oad file__: Pauses the simulation and prompts for a simulation file.
   
__[S]creenshot__: Pauses the simulation and prompts for a file into which the screenshot will be saved. Screenshot files are plaintext.
   
__e[X]it__: Exits the program.

### The Model Description Language

#### Quick Introduction

Descriptions of models are essentially lists of state transitions. For example, if a water cell becomes an ice cell half the time it’s next to an ice cell and becomes an air cell 1/100 of the time it’s next to more than 3 air cells, the rules are written:

```
(trans 'water (* (neighbor 'ice) 0.5)) 'ice)
(trans 'water (* (neighbor> 'air 3) 0.01) 'air)
```

The format of this is `(trans current-state transition-probability new-state)`. Helper functions such as `neighbor` return __1__ or __0__ for true or false conditions, and can be easily integrated into a more complex function.

A fractional probability isn’t required, however; for Life, e.g., the transitions would be:

```
(trans 'full  (neighbor< 'full 2) 'empty)
(trans 'full  (neighbor> 'full 5) 'empty)
(trans 'full  turns-into 'full) ;; this isn't necessary when the state doesn't change
(trans 'empty (neighbor= full 3) 'full)
(trans 'empty turns-into 'empty) ;; this is also unnecessary, but added for explicitness
```

The description of the simulation also contains its attributes: its dimensions, the “neighborhood” of a cell (whether diagonal neighbors are included or not), the starting state of the world, etc. For the predator-prey simulation above, we could have:

```
(world :height 25
       :width 40
       :start-proportions (('🐺 0.10) ('🐰 0.3333)))
```

#### File Format

Please use `.sca` as the extension. These are proper lisp files and can be matched to it for syntax highlighting in editors. Moreover, all the code will be run (or not, if there's an error!)

#### A Full BNF Notation

*config* ::= *title* *description* _trans*_ *world*  
The config file contains a title, descriptive information about the simulation, a list of transitions, and configuration info for the "world."

*title* ::= (title \<string\>)  
*description* ::= (description \<string\>)  
The title and description are just strings.

##### Transitions

*trans* ::= (trans *current-state* *transition-probability* *new-state*)  
From *current-state*, turn into *new-state* with probability *transition-probability*.

*current-state* ::= *state-symbol*  
*new-state* ::= *state-symbol*  
*state-symbol* ::= \<symbol\>  
There is one special symbol, 'empty, which displays as a blank space. Otherwise the state is displayed as the first character of the state's name. Use emoji...

*prob* ::= 0 | 1 | \<s-exp\> | \<s-exp using a helper function\>  
*prob* must evaluate to a number between 0 and 1 inclusive.

##### Helper Functions

*helper* ::= *neighbor-helper* | turns-into  
turns-into is a synonym for 1.

*neighbor-helper* ::= (neighbor *state-symbol*) | (neighbor= *state-symbol* \<number\>) | (neighbor< *state-symbol* \<number\>) | (neighbor> *state-symbol* \<number\>) | (neighbor<= *state-symbol* \<number\>) |  (neighbor>= *state-symbol* \<number\>)  
neighbor returns 0 or 1 based on at least one *state-symbol* located adjacent to the cell.  
neighbor=, neighbor<, etc. return 0 or 1 based on having \<number\> of *state-symbol*'s located adjacent to the cell.

##### Simulation Settings
		      
*world* ::= (world &rest &optional _start-properties*_ *start-configuration*)  
This is not required.

*start-properties* ::= *dimensions* | *proportions*
*dimensions* ::= :dimensions (\<height\> \<width\>)  
Defaults to ()screen-height - 1 screen-width).

*proportions* ::= :proportions ((*state-symbol* *proportion*)*)  
A list of states and their overall proportions. When the simulation is created, cells will be randomly assigned to these states. 'empty is allowed. *proportion* is a number between 0 and 1; the sum of all *proportion*'s should be no more than 1.  
This defaults to all 'empty.

*start-configuration* ::= :start-config \<string\>  
A multi-line string.  

#### Editors and Terminals

Dealing with emoji (and more importantly, other non-ASCII-width characters) is not the strong suit of current fixed-pitched displays (cf. [Adventures in Emoji]()https://ianrenton.com/blog/adventures-in-emoji/), [When monospace fonts aren't](http://denisbider.blogspot.com/2015/09/when-monospace-fonts-arent-unicode.html)).

If you want to use emoji in creating your models (and I recommend you do), it's best to use *only* emoji, or the display will be messed up.

If emoji aren't displaying in your terminal:

* If there's a setting for "Unicode East Asian characters...", make sure it's selected.
* If you can choose a second font, add one with emoji characters.

If emoji aren't displaying in Emacs:

* add `(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)` to your .emacs file. An excellent alternate font is Symbola.


## Models

### Examples

Additional examples are included in the /simulations subdirectory.

[Snowfall](./simulations/snowfall.sca) is a model of snow melting. It uses three states and nine transitions.

[Life](./simulations/life.sca) is a version of Conway's Game of Life.

### Other Models

* The [Schelling Model of Segregation](http://nifty.stanford.edu/2014/mccown-schelling-model-segregation/)
* Non-random Cellular Automata. There are some famous examples of this:
  * [Life](http://www.bitstorm.org/gameoflife/)
  * von Neumann's universal constructor
  * [Langton's Ant](https://en.wikipedia.org/wiki/Langton%27s_ant) and [Turmites](https://en.wikipedia.org/wiki/Turmite)
  * [Wireworld](https://en.wikipedia.org/wiki/Wireworld)
  * [Biham–Middleton–Levine Traffic Model](https://en.wikipedia.org/wiki/Biham–Middleton–Levine_traffic_model)
* [Sugarscape](http://groups.engin.umd.umich.edu/CIS/course.des/cis479/projects/*sugarscape/Artificial%20Life%20web.htm)
* Alan Turing's [Leopards' Spot Problem](http://shell.cas.usf.edu/~stark/leopardsEssay.html)
* [DaisyWorld](https://en.wikipedia.org/wiki/Daisyworld)
