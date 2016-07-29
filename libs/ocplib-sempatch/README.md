# ocplib-sempatch

A semantic patches library

## Overview

`ocplib-sempatch` is a library which allows you to detect and replace specific
constructs in your code, using so called "semantic patches"

## Patching process

Each patch is converted to an automaton which is then matched against the
Parstree.

The structure of the automaton is generated during the compilation from the
definition of the Parsetree, thanks to the (really ad-hoc) ppx "abc" (in
lib/automaton/generator). This ppx also generates the functions used to
convert the patch into an automaton and to evaluate that automaton.

When the automaton is done running, the matches are filtered through the guard,
which is a boolean expression taking as input the metavariables of the match
(it is used to add some conditions which can't be expressed in the automaton,
like the equalilty of two variables).

## TODO

(At least the most important things)

### Rewrite the automaton

The current automaton has two flaws :

- It is a top-down automaton, which makes getting the AST after transformation
  quite hard for some reasons

- It has a ugly design and needs to be rewritten if we want to be able to optimize it.

A rewrite has begun
[here](https://github.com/ocamlpro-Hufschmitt/typerex-lint/tree/bup/libs/ocplib-sempatch/lib/automaton),
but the generation from the Parsetree han't been finished (but almost...).

The idea of the generator is that it reads the cmi of the parsetree from
current compiler (currently whith a hardcoded path which probably doesn't exist
in your computer), and uses the type declaration in it to generate a lot of
stuff. (more or less every module in libs/ocplib-sempatch/lib/automaton/abc is
a generator for some part of the code).

What remains to do is mostly to generate the evaluator and add special cases
for nodes that have to be treated in a special way (like Locations which have
to be ignored and special annotations).

### Allow patching more than expressions and structures

For arbitrary reasons, the library only allows modifying expressions or
structures (in the sense of the type Parsetree.structure).

This is absurd as everything is here to allows patching every element of the
AST, but changing it would need to rethink a little the API.

### Pretty-print the modified AST

This could be quite useful, but a rewrite of the automaton is probably needed
to get the full modified AST.

### Extend the grammar and clarify it

A lot of things to do...

#### Add disjunctions

Allows writing patches like this (if possible whith a less ugly syntax)

```
x
(
=
|
==
)
y
```

which detects `x = y` and `x == y`

This can be really handy when used in the linter
