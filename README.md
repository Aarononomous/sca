# Modeling Worlds with Stochastic Cellular Automata

## Overview

We can create and model sophisticated models of worlds with a very simple mechanism: within the world (described as a grid of cells), the state of each cell is updated based solely upon its current state and the states of its neighbors.

This model is called __cellular automata__. A notable example of this is the Game of Life, invented by John Conway in 1970. From only four rules, powerful and surprising behavior emerges. Even without direct interaction between entities, group organization seems to be present.

By introducing randomness to the state changes (the “stochastic” in “stochastic cellular automata”), even simpler models can display extremely complex behavior.

This project develops

1. a simulator to run these models in;
2. a language for describing models and worlds; and
3. a bunch of fun models to play with.

## More

A really wonderful explication of this can be seen (and played with) at [Simulating the World (in Emoji)](http://ncase.me/simulating), and I urge everybody to do so.

To introduce stochastic cellular automata, let’s start by constructing a simple model. The [predator-prey equations](https://en.wikipedia.org/wiki/Lotka–Volterra_equations) describe cyclical changes in population. We’ll create another model of this relation with a different mechanism, cellular automata. There are three states in the model: empty, wolf, and bunny. Let's devise transition rules for each of these states.

### Empty cells:

* if more than two neighbors are bunnies (50% of the time) ⟶ become a bunny; __reproduction__
* if more than two neighbors are wolves (25% of the time) ⟶ become a wolf; __reproduction__

### Bunny cells:

* if one neighbor is a wolf ⟶ become empty; __predation__
* if more than one neighbor is a wolf ⟶ become a wolf; __predation and reproduction__
* if more than three neighbors are bunnies ⟶ become empty; __overconsumption__

### Wolf cells:

* if less than one of its neighbors is a bunny ⟶ become empty; __starvation__

And that's it!

We set up the "world" with a random assortment of cells and begin the simulation. After 10 or so generations, patterns begin to emerge. Small groups of wolves are formed, but soon surround themselves with empty cells by consuming all the bunnies in the region, leading to “packs” roving around the world leaving empty patches in their wakes, which are then re-filled with bunnies after another dozen generations. From a random state, as well, stable population proportions emerge, while epicyclic waves of population density come and go.

## Implementation

### The Simulator

The terminal window is used as the display. With the exception of the activity bar which occupies its bottom line, the entire window displays the simulation. With each generation, all the cells within the model are updated and the world is redisplayed.

Controls are simple keystrokes, and a key is always displayed on the activity bar:

__`[P]ause/Play`__: Does the obvious thing.

__`[L]oad file`__: Pauses the simulation and prompts for a file.
   
__`[S]creenshot`__: Pauses the simulation and prompts for a filename and location to save the screenshot to. Screenshots are simple text files.
   
__`e[X]it`__: Exits the program.

### The Model Description Language

#### Quick Introduction

Descriptions of agents are effectively lists of state transitions. For example, if a water cell becomes ice half the time it’s next to an ice cell, and becomes air 1/100 of the time it’s next to more than 3 air cells, the rules are written:

```
(TRANS 'water (* (NEIGHBOR 'ice) 0.5)) 'ice)
(TRANS 'water (* (NEIGHBOR> 'air 3) 0.01) 'air)
```

The format of this is `(TRANS current-state transition-probability new-state)`. Additional parts of the agent description handle its representation, a text description, and so on.

`neighbor` is a helper function, part of a family of related functions that inspect the neighbors of a cell.
A fractional probability isn’t required; for Life, e.g., the transitions would be:

```
(TRANS 'full  (NEIGHBOR< 'full 2) 'empty)
(TRANS 'full  (NEIGHBOR> 'full 5) 'empty)
(TRANS 'full  TURNS-INTO 'full) ;; this isn't necessary when the state doesn't change
(TRANS 'empty (NEIGHBOR= full 3) 'full)
(TRANS 'empty TURNS-INTO 'empty) ;; this is also unnecessary, but explicated
```

The description of the simulation is a list of attributes : its dimensions, the “neighborhood” of a cell (whether we include the diagonal neighbors or not), the starting state of the world, whether the world wraps around the edges of the grid, etc. For the predator-prey simulation from above:

```
(WORLD :HEIGHT 25
       :WIDTH 40
       :START-PROPORTIONS (('wolf 0.10) ('bunny 0.3333)))
```

#### More

TODO: A full description and BNF notation


Helper functions: TURNS-INTO  -> 1
       		  NEIGHBOR   -> {0,1}
		  NEIGHBOR=  -> {0,1}
		  NEIGHBOR<  -> {0,1}
		  NEIGHBOR>  -> {0,1}
		  NEIGHBOR<= -> {0,1}
		  NEIGHBOR>= -> {0,1}


The BNF notation:

```
config ::= title description state* world
title ::= (TITLE <string>)
description ::= (DESCRIPTION <string>)
state ::= (STATE current-state cond new-state)
current-state ::= state-symbol
new-state ::= state-symbol
state-symbol ::= <symbol>
cond ::= [0,1] | (s-exp) | (s-exp ... [helper] ...) ;; a number between 0 and 1
helper ::= neighbor-helper | TURNS-INTO
neighbor-helper ::= (NEIGHBOR   state-symbol)   |
        	    (NEIGHBOR=  state-symbol <number>) |
		    (NEIGHBOR<  state-symbol <number>) |
		    (NEIGHBOR>  state-symbol <number>) |
		    (NEIGHBOR<= state-symbol <number>) |
		    (NEIGHBOR>= state-symbol <number>)
world ::= (WORLD start-properties start-configuration)
start-properties ::= (&rest &optional dimensions proportions start-configuration)
dimensions ::= :DIMENSIONS (height width)
proportions ::= :PROPORTIONS ((cell1 [0,1]) (cell2 [0,1]) etc.)

????
'EMPTY
start-configuration ::= :START-CONFIG <string>
```

## Models

TODO: Should these be actual files? Yes, right?
      There should be three descriptions: files, examples, and links.

### Examples

TODO

### Prose Descriptions of Models

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
