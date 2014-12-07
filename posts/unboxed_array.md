---
title: The Missing Unboxed Vector Library
date: Dec 06, 2014
---

### The Missing Unboxed Array Library

Reflecting on the current state of numeric Python ecosystem and discussing this
problem space with other practitioners it has become more evident to me that
there is a need for a currently missing library which provides not a full array
and dense linear algebra functionality but something much more basic, simply an
unboxed vector library with efficient indexing, traversal and creation as well
as a mapping between the storage layer C types and regular PyObjects per some
small schema language. A superset of this functionality is provided by the
popular NumPy library although the ubiquity of NumPy being cast into a role for
a subset of it's use-cases is problematic for several reasons.

Quite a few libraries like Pandas, various ETL frameworks, or data visualization
libraries end up pulling in a full NumPy dependency simply to access a subset of
the functionality provided by it and as the most accessible space efficient way
of storing data. While NumPy is well-suited for scientific practitioners, the
dependency on linking against Fortran libraries to access linear algebra
routines introduces an unnecessary complication for downstream libraries that
are simply pulling it in to access the unboxed vector structure to build higher
level structures like dataframes which are effectively just
heterogeneously-typed same-length vectors with row and index information
attached.

The functionality needed on top of these vectors is simply logic pertaining to
basic arithmetic, statistical aggregations and operations like joins in a way
that doesn't need to round trip between the C layer and the Python interpreter
for every operation. This in itself hints the need for a robust way of building
domain specific query languages that we've danced around with metaprogramming
hacks for years. But I feel that a foundational array representation that is
the least-common denominator for a wide variety of use-cases is needed to even
begin work on the next level of abstraction of an expression compiler.

I write this blog post as sort of a litmus test to see if other people feel
the same about this missing abstraction and feel that it would be worth
devoting time to developing a small Python C extension to this end.
