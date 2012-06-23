# union-find

A simple Haskell library that implements Tarjan's Union/Find
algorithm.  Useful, for example, to implement unification in a type
inference system.

The Union/Find algorithm implements these operations in
(effectively) constant-time:

 1. Check whether two elements are in the same equivalence class.

 2. Create a union of two equivalence classes.

 3. Look up the descriptor of the equivalence class.


## Installation

Using `cabal` (which comes with the Haskell Platform):

    $ cabal install union-find

or in the checked-out repository:

    $ cabal install
