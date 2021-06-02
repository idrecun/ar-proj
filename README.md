# tseyTiny

tseyTiny is a simple tool written in Haskell which can convert a propositional
logic formula into an equisatisfiable 3-CNF formula. The conversion is done
using Tseytin transformations, as implied by its name, and the resulting 3-CNF
formula is linear in size with respect to the size of the input formula.


## Installation

The tool can be built using the `ghc` Haskell compiler

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

### Example 1

`./tseyTiny`

Input

`q => p | ~r`

Output

```
c (q => (p | ~r))
c [("$1",1),("$2",2),("$3",3),("p",4),("q",5),("r",6)]
p cnf 6 9
3 0
-3 -5 2 0
5 3 0
-2 3 0
-2 4 1 0
-4 2 0
-1 2 0
-1 -6 0
1 6 0
```

### Example 2

The input can be read from a file instead by redirecting the standard input.
The same can be done for the output.

`./tseyTiny < input.txt > output.cnf`

input.txt

`(q & true) | ~~r`

output.cnf

```
c ((q & true) | ~~r)
c [("$1",1),("$2",2),("$3",3),("q",4),("r",5)]
p cnf 5 8
3 0
-3 4 2 0
-4 3 0
-2 3 0
-2 -1 0
2 1 0
-1 -5 0
1 5 0
```

## Tseytin transformation

Suppose a propositional formula `F` is of the form `F = A & B`. Then `F` is
satisfiable iff the set of formulas `{S, S <=> A & B}` is satisfiable. This can
be rewritten (by simplifying the second formula to CNF) as `{S, ~S & A, ~S & B,
S & ~A & ~B}` which is basically a 3-CNF formula. This can similarly be done
for any (binary or unary) operator.

When `A` and `B` are subformulas instead of variables, the Tseytin
transformation is the procedure of recursively renaming subformulas `A` and `B`
by introducing the previously mentioned equivalences and adding those
equivalences to the resulting set of formulas.

For example, the formula `p & q => q | ~r` can be transformed in the following
way
```
First transform the left side of => recursively
  p & q -> {S1 <=> p & q}
Then transform the right side
  q | ~r
  First transform the left side of | recursively (nothing to do, q is a variable)
  Then transform the right side
    ~r -> {S2 <=> ~r}
  Now the formula is q | S2, so finally
  q | S2 -> {S3 <=> q | S2}
Now the formula is S1 => S3, so finally
S1 => S3 -> {S4 <=> S1 => S3}

The 3-CNF can be constructed from the following set of formulas
{S4, S4 <=> S1 => S3, S3 <=> q | S2, S2 <=> ~r, S1 <=> p & q}
```

Clearly, the size of the resulting set of formulas is linear with respect to
the size of the input formula. This bypasses the exponential bound of
algebraically transforming the input formula to an equivalent CNF, thus
allowing for a practical usage of SAT solvers.
