Star Battle Solver by Mike Machenry
---

This is a solver for the Star Battle puzzle that I originally wrote for
Mystery Hunt 2017.

I noticed that a traditional backtracking solver took far too long (more than a
day) too solve a 10x10 Star Battle. I decided to rewrite it using constraint
logic programming on finite domains, or CLP(FD). It now solves the 10x10 in
about a minute and thirty seconds.

TODO
---

* Create documentation and a test suite.

* Move the region detection into the runFD to increase performance.

* Consider parallelizing solution generation. A good strategy for this might
  be to provide an argument to generate that constrians the most significant
  digit in the puzzle generation to be a particular number. It could then be
  run in parallel with each possible argument.

* Parameterize the board size.

* Create a file reader that reads regions from a file in an easy-to-write
  format. Check that it's a valid board.

