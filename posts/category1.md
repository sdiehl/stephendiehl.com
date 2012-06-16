---
title: Categorical Programming: Introduction
date: September 8, 2012
---

### Forward

This is a multipart blog post on the structure of the pure mathematical
field of Category theory and and how it relates to real world
programming in Haskell.

Category theory distills the essense of a large variety of
constructions in classical set theory out to more abstract
context which can be used to reason about the large scale
structure of other fields and mathematics, physics, and computer
science.

In a way some of the constructions in category theory are almost "too
abstract" since they are often model very low-level universal properties
of mathematics. This does often leave one questioning, what does this
construction really mean? One migt be tempted to then try to go back to
concrete set theory exmaples, but in doing so one is often distracted by
the example and looses the insight essential nature of the theory.

In short, often times the best intuition is derived from looking
at the constructions in their full abstraction, i.e. that they
are nothing more than abstract symbols and processses of
manipulation on those symbols.

### Categories

A *category* is defined as a collection of three things:

1. A collection of **objects**.
2. A collection of **morphisms**.
3. An **composition** operation (.) which has the following property:
4. For each object A there is an identity morphism $ id_A $.

Which all obey the following properties:

**Associativity**

\[
    ( f . g ) . h = f . ( g . h )
\]

**Identity**

\[
    f . id_A  = id_A . f = f
\]

**Composition Constraint**

Morphisms also have associated objects $ A,B \\in C $ referred to as domain
and codomain written $ dom(f) $ and $ cod(f) $.

If $ f . g $ is well defined then:

\[
dom(f) = cod(g)
\]

> Morphisms are not functions. It is the other way around, in the
> category of SET functions are morphisms with objects as sets but this
> is a a special case. In general morphisms are a pure abstraction which
> is structurally similar to functions.

#### SET

Categories are often written in bold. For example a very common
category is that of SET.

1. Classical sets as objects.
2. Total functions as morphisms.
3. A composition operation $ \\circ $.

\[
g \\circ f = \\{ (x, g(f(x))) | x \\in A \\}
\]

4. For each set $A$ there is an identity function $ id_A $ which
maps the set $A$ to itself $ f(A) = A $.

With the usual properties:

\[
    ( f \\circ g ) \\circ h = f \\circ ( g \\circ h )
\]

\[
    f \\circ id_A  = id_A \\circ f = f
\]

In SET domain and codomain are sometimes referred to as domain and range.


#### HASK

The set of all non-polymorphic Haskell types forms a category
with Haskell functions as morphisms.

```haskell
head :: [a] -> a
head (x:_) =  x
```

\

HASK also contains both a composition operator and a identity the
Prelude.

```haskell
(.)              :: (b -> c) -> (a -> b) -> a -> c
f . g            =  \ x -> f (g x)
```

```haskell
id               :: a -> a
id x             =  x
```

\

The the constraint on domain and codomain alignment is enforced
by the type checker.


#### VEC

For the linear-algebra inclined, a space of vectors with linear mappings
between vector spaces also forms a category with.

1. Vectors spaces as objects.
2. Linear mappings as morphisms.
3. A composition operation $ \\circ $ composing linear mappings.
4. For each vector $ A $ there is an identity mapping $ id_A $.

#### CAT

In the original definition of category we did not mention that the
objects or arrows neccesarily form a set. Instead we stated in the
definition that we need only have a collection of objects and arrows.

A category which has its arrows and objects in a set is referred to as a
**small category**. A category which does not is referred to as a
**large category**.

This begs the question as to whether we can form the category
of all categories. The answer to this is no, because of
the implications of self-referential sets. See [Russell's
Paradox](http://en.wikipedia.org/wiki/Russell's_paradox).

It is however very illuminating to look at the category of all small
categories called **CAT** which does indeed form a category with objects
being categories and "mappings" called functors as morphisms between
categories.

The category of categories is where category theory starts to
become interesting. More on this in the next post...
