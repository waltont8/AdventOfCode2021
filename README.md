# AdventOfCode2021
Advent of Code 2021 https://adventofcode.com/2021

## Day 1: Sonar Sweep
Just zipwith the list of numbers with itself using (>) to get a list of bools. Transpose and sum on part 2 to add the lists. There is probably a nicer way of doing part 2 using Control.Applicative to add the lists, I'll figure that out later. Most of these I do a quick answer then go and find nicer ways to do it much later on.

## Day 2: Dive!
Was warming up my integer trig package when I read "aim" in part 2, but was a simple addition to part 1 in the end. Bodged the read function, which is ugly, but a lot faster to type than doing the proper invertible syntax descriptions one

## Day 3: Binary Diagnostic
This was a step up in difficulty from day 2. I found it quite hard in Haskell. For Haskell I need to have the whole picture in my head to put out a good solution and there was a lot going on in the puzzle compared to 1 and 2. Part 2 didn't feel like something I could work through iteratively (not whilst trying to be quick, anyway!)

## Day 4: Giant Squid
For real, the thing that took the longest here was that I forgot haskell can't tell the difference between -1 and - 1. Confusing early in the day! I guessed that part 2 would punish hackery on part 1 so I took a bit longer and, hurrah, got part 2 for free.

## Day 5: Hydrothermal Venture
Big hint in part 1 that part 2 was going to add diagonal lines. Would have been interesting to see arbitrary lines but I suspect there is more than one valid way to draw such a line, also it would have been really hard to resist anti-aliasing them all. 
<p align="center">
  <img src="output.png" width="550" title="Vizualization of Day 5">
</p>
