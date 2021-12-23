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

## Day 6: Lanternfish
When it says exponentially in bold, don't brute force it! Glad it was a short one for Monday morning.

## Day 7: The Treachery of Whales
That was the shortest one yet. I fear for what's coming :)

## Day 8: Seven Segment Search
Part 1 was quick. Took a long time for part 2. I think, you could just brute force it and find the combinations that make sense, which sounds quicker to write. I might have a go later. I'm happy with my part 2, it was just slow to build.

## Day 9: Smoke Basin
There are better ways to do this using monads to pass the state around, but at 7am I'm just going to move it all around manually.
<br>**Edit** : Ok, I rewrote part 2 in the state monad. Wasn't too hard (and it's still a bit of a mess) but required more thinking than the original. I will need to write a lot of these before I can use them as quickly as bodging it with C-like Haskell :)

## Day 10: Syntax Scoring
Not too difficult today, push opens onto a stack and then check its reverse is the next closing character. Part 2 was easy which usually means there is a quick hack for part 1 that I didn't use :) . 25 years of trying to pass code reviews has made it quite hard to find the hacky solution, I usually have a mental eye out for test cases and what comes next.

## Day 11: Dumbo Octopus 
Quite straightforward, wasted quite a lot of time on a ridiculous bug. Again, I threaded data through a complicated structure by hand rather than keeping it in a monad which I think is the solution I would try with more time. Clearly the thing to practice for next year is passing state around quickly the right way.

## Day 12: Passage Pathing
Took quite a while to do this one, got lost in part 1 tracking which nodes had been visited and initially only let a node be visited once in *any* walk but obviously it's only once per individual walk. Second part was quite straightforward after that although I stopped to tidy up the mess before doing it. For part 2 I generated all possible graphs where a single lower case node can be visited twice, walked them all and merged the result. Slow, but very easy to think about when you're trying to hurry up.

## Day 13: Transparent Origami
I really like the ones where you draw out a code in big letters.
```
###   ##  #### ###   ##  ####  ##  ###
#  # #  #    # #  # #  # #    #  # #  #
###  #      #  #  # #    ###  #  # ###
#  # #     #   ###  #    #    #### #  #
#  # #  # #    # #  #  # #    #  # #  #
###   ##  #### #  #  ##  #### #  # ###
```

## Day 14: Extended Polymerization
You knew part 2 was going to be part 1 but too hard to do the simple way. I still did part 1 the simple way as it was easy and just in case part 2 wasn't what I was expecting. It feels a bit "stream of consciousness" when I look back at the code but other than cleaning up unused wrapper functions I'm not going back and tidying up any of these.

## Day 15: Chiton
So this felt a bit too cheaty. It's a maze problem and Haskell has astar and dijkstra in the Algorithm.Search module, so that takes care of that. Getting the cost data in would have been tricky so I just copied the input in as a constant string inside the cost function. I guessed part2 would be either a tiling of the map or a new set of directions so I made sure the map was tiled just in case, paid off. All I really had to do for part 2 was change the number of tiles and implement the wrap back to 1 function. Bit messy, bit cheaty, but part 2 took about 10 seconds. Might come back to this one and implement something a bit cleaner.

## Day 16: Packet Decoder
I kept thinking about using one of the parsers for this but I think it would have taken longer. Many LoC in this solution, but a lot of it is pattern matching cut and paste to convert strings and numbers. Certainly this could be more compact, but I don't think it could be more obvious and I'll take obvious over compact over compact any day.

## Day 17: Trick Shot
Feels like there was probably a smarter way to do this than just trying everything. Was really tempted to ditch Haskell for this one and make a visual version with a slider that lets you adjust the velocity by hand, probably using processing.org. Might come back to it.<br>
**Edit** : I had a look at some visualizations for this, they don't look great. The aspect ration is 1:50. Interesting that the probe curves in the opposite direction that you would expect as vx drops to zero before vy flips.

## Day 18: Snailfish
That explode function took a lot of debug, I was going down the wrong side of the right side. I wonder if you can capture left and right numbers in the type system where you could see deliberate moves but catch accidentally passing a left to a right? Something to ponder.

## Day 19: Beacon Scanner
This was a tough one. My first solution was done very quickly, I generated all possible rotations and positions of everything and filtered it for the right answers hoping that haskell's lazy magic would save me. It often does, it didn't this time. So I rewrote it all as what was basically for loops with break statements and that worked ok (although it still takes nearly a minute to finish on my laptop). Then part 2, which just did not work with any of my solutions, so rather than start again I just hacked something in with constants. All the stuff about 100000 is just me tracking centrepoints with known values so I can get part 2. What a mess!

## Day 20: Trench Map
Two things caught me today. One, the fact the infinite space flashes on and off. Not hard to deal with but in my head that was all blank until it got touched. The second thing is this, copied here from ghci:
```
*Lib> minimum [0,2]
0
*Lib> minimum [2,0]
0
*Lib> minimum (2,0)
0
*Lib> minimum (0,2)
2
*Lib>
```
Thanks, haskell!

## Day 21: Dirac Dice
Part 1 was easy but fiddly. Part 2 I was quite worried about, given the large numbers, but throwing everything you've already seen into a map seemed to work. I had to re-use the state monad code from an earlier day to get the map of seen states to the code that needs it, which then meant everything was in a do block which is quite awkward. I didn't want the case statements in there but couldn't see a quick and easy way of getting rid of them. Nearly fell into that stupid max tuple hole from yesterday again.

## Day 22: Reactor Reboot
Pretty obvious what was coming in part 2 but I thought it best to get the points from part 1 quickly and then fight part 2. Spent quite some time fiddling about with the cube splitter until I realised you could just use a list comprehension to generate all sections and then drop the overlapping one. That shrunk a page of code into a few lines! I think you could do this with some sort of tree that recursively subdivides space which might be nice.

## Day 23: Amphipod
I woke up very early this morning, didn't fancy another graph walk/optimization thing. Decided to just do this one by hand and wrote a program to help. I used processing.org to build the UI, can't have taken me more than 20 minutes. Processing is awesome for quick visual hacks.
