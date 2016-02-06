# Modeling Worlds with Stochastic Cellular Automata

## Overview

We can create and model sophisticated models of worlds with a very simple mechanism: within the world (described as a grid of cells), the state of each cell is updated based solely upon its current state and the states of its neighbors.

This model is called **cellular automata**. A notable example of this is the Game of Life, invented by John Conway in 1970. From only four rules, powerful and surprising behavior emerges. Even without direct interaction between entities, group organization seems to be present.

By introducing **randomness** to the state changes (the “stochastic” in “stochastic cellular automata”), even simpler models can display extremely complex behavior.

My proposal is to develop 1) a simulator to run these models in; 2) a language for describing models and worlds; and 3) a bunch of fun models to play with.

## More

A really wonderful explication of this can be seen (and played with) at [Simulating the World (in Emoji)](http://ncase.me/simulating/), and I urge everybody to do so.

To illustrate stochastic cellular automata, let’s construct a simple model. The predator-prey equations describe cyclical changes in population. We’ll model this relation in a different way: with cellular automata. With three states: empty, wolf, and bunny, we create transition rules for each state.

**Empty cells:**

- if more than two neighbors are bunnies (50% of the time) ⟶ become a bunny; **reproduction**
- if more than two neighbors are wolves (25% of the time) ⟶ become a wolf; **reproduction**

**Bunny cells:**

- if one neighbor is a wolf ⟶ become empty; **predation**
- if more than one neighbor is a wolf ⟶ become a wolf; **predation and reproduction**
- if more than three neighbors are bunnies ⟶ become empty; **overconsumption**

**Wolf cells:**

- if less than one of its neighbors is a bunny ⟶ become empty; **starvation**

We set up the world with randomized cells and begin the simulation. After 10 or 0 generations, patterns begin to emerge. Small groups of wolves are formed, but soon surround themselves with empty cells by consuming all the bunnies in the region, leading to “packs” roving around the world leaving empty patches in their wakes, which are then re-filled with bunnies after another dozen generations. From a random state, as well, stable population proportions emerge, while epicyclic waves of population density come and go.

## Implementation

### The Simulator

The terminal window is used for display. With the exception of a status bar on the bottom line, the entire window will display the simulation. At every generation, each cell within the grid updates and the world is redisplayed.

Controls for pausing/resuming and resetting the simulation can be entered. From the command line, it will also be possible to load simulation description files and save “screenshots.”

### The Model Description Language

The descriptions of agents are primarily lists of state transitions. For example, if a **water** cell becomes **ice** half the time it’s next to an **ice** cell, and becomes **air** 1/100 of the time it’s next to more than 3 **air** cells, the rules would be:

```
(water ((neighbor ice) 0.5) ice)
(water ((> (neighbor air) 3) 0.01) air)
```

The format of this is `(state neighborhood new-state)`. Additional parts of the description handle its representation, a text description, and so on. A transition probability isn’t necessary; for Life, e.g., the transitions would be:


```
(full (< (neighbor full) 2) empty)
(full (> (neighbor full) 5) empty)
;;; etc.
```

There are additional parameters for the simulation as a whole: its dimensions, the type of “neighborhood” around a cell (whether we include the diagonal neighbors or not), the original state of the world, whether the world wraps around the edges of the grid, etc.

```
(world :height 25
       :width 40
       :neighborhood 'von-neumann
       :start-proportions ((reds 0.25) (blues 0.25) (empty 0.5))
       ...)
```
	
If anybody can suggest a name for this language, I’d appreciate it!

### Models

* The Schelling Model of Segregation
* *Non*-random Cellular Automata. There are some well-known examples.
  * [Life](http://www.bitstorm.org/gameoflife/)
  * John von Neumann’s Universal Constructor
  * [Langton’s Ant](https://en.wikipedia.org/wiki/Langton's_ant) and [Turmites](https://en.wikipedia.org/wiki/Turmite)
  * [Wireworld](https://en.wikipedia.org/wiki/Wireworld)
  * The Biham–Middleton–Levine Traffic Model
* [Sugarscape](http://groups.engin.umd.umich.edu/CIS/course.des/cis479/projects/*sugarscape/Artificial Life web.htm)
* Alan Turing’s [Leopards’ Spot Problem](http://shell.cas.usf.edu/~stark/leopardsEssay.html)
* [DaisyWorld](https://en.wikipedia.org/wiki/Daisyworld)

Some of these will require additions to the model, and are stretch goals. For example, DaisyWorld requires the world to have state, a “temperature.”

## Stretch Goals

If I complete this project ahead of schedule, I *already* have a list of improvements.

Add state to the world; then I can do DaisyWorld.

Add additional state to cells.

Add rules for specific neighbors (top, left, etc.). Then I can implement a good-looking version of Wolfram’s [Rule 110](https://en.wikipedia.org/wiki/Rule_110).

Make sure the config files are very readable.

Better updating. Looping over cells that won’t change is a real waste of time. I can come up with an algorithm which only updates cells that *have* changed.

Make worlds vastly larger than the terminal, and pan around them. This and the previous makes it reasonable to create automata that compute.
