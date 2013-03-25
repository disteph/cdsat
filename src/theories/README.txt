---------------------
Writing a new theory.
---------------------

A theory should implement TheoryInterface as defined in the file TheoryInterface.mli

In particular, it should contain:
- an implementation Atom of AtomType defining the notion of literals manipulated by the kernel
- the implementation of a functor DecProc that takes as input an implementation of sets of literals, and produces 2 functions deciding the consistency of a given set of literals
- the implementation of a functor Parser that takes as input an implementation of formulae, and produces a parsing function that turns a filename into a formula (it should also produce a list of example formulae for the theory)



Hygiene policy:
- Write your theory in a subdirectory XXX of the present directory
- It should contain a module MyTheory (e.g. a file myTheory.ml), implementing TheoryInterface
- Add an mlpack file to package the directory into a module XXX
- Edit the main file main.ml to add your theory to the list of known theories
- To compile, do not forget to add to the top-level _tag file an entry to include your directory src/theories/XXX for pack XXX
