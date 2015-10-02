# Functional Pearl: Getting a Quick Fix on Comonads

## Abstract

A piece of functional programming folklore due to Piponi provides
Lob’s theorem from modal provability logic with a computational
interpretation as an unusual fixed point. Interpreting modal necessity
as an arbitrary Functor in Haskell, the "type" of Lob’s theorem is
inhabited by a fixed point function allowing each part of a structure
to refer to the whole.
However, Functor’s logical interpretation may be used to prove
Lob’s theorem only by relying on its implicit functorial strength,
an axiom not available in the provability modality. As a result, the
well known loeb fixed point "cheats" by using functorial strength to
implement its recursion.
Rather than Functor, a closer Curry analogue to modal logic’s
Howard inspiration is a closed (semi-)comonad, of which Haskell’s
ComonadApply typeclass provides analogous structure. Its computational
interpretation permits the definition of a novel fixed point
function allowing each part of a structure to refer to its own context
within the whole. This construction further guarantees maximal sharing
and asymptotic efficiency superior to loeb for locally contextual
computations upon a large class of structures. With the addition of a
distributive law, closed comonads may be composed into spaces of
arbitrary dimensionality while preserving the performance guarantees
of this new fixed point.
From these elements, we construct a small embedded domainspecific
language to elegantly express and evaluate multidimensional
"spreadsheet-like" recurrences for a variety of cellular automata.

[Link to the paper](https://andorp.github.io/asset/articles/GQFC.pdf)
[Original link](https://github.com/kwf/GQFC/blob/master/GQFC.pdf)
