# tseyTiny

tseyTiny is a simple tool written in Haskell which can convert a propositional
logic formula into an equisatisfiable 3-CNF formula. The conversion is done
using Tseytin transformations, as implied by its name, and the resulting 3-CNF
formula is linear in size with respect to the size of the input formula.


## Installation

The tool can be built using the `ghc` Haskell compiler:

`ghc --make Main.hs -o tseyTiny`


## Usage

tseyTiny can be called from a command line interface. It accepts a
propositional logic formula from the standard input and produces a DIMACS
formatted 3-CNF formula to the standard output.

The grammar of the input formula is specified below.

```
Variable is a string of one or more lowercase or uppercase alphabetical
characters

Const   -> true
         | false

Formula -> Formula <=> Formula
         | Formula  => Formula
         | Formula  |  Formula
         | Formula  &  Formula
         | ~ Formula
		 | ( Formula )
         | Const
         | Variable

All whitespace characters are ignored. The operators are given in order of
their precedence from lowest to highest. Binary operators `<=>` and `=>` are
right-associative, while `|` and `&` are left-associative.
```

The output is formatted following the DIMACS specification. The first line of
the output is a comment line containing the parsed input formula. The second
line is a comment line containing a mapping of the variable names from the
input formula (and the ones artificially produced during the Tseytin
transformation) to their DIMACS enumerated counterparts. The rest of the output
contains the produced 3-CNF formula.

The input can be read from a file instead by redirecting the standard input.
The same can be done for the output.

Example usage:

`./tseyTiny <formula.txt >output.cnf`



## The Tseytin transformation

## References
