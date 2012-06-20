---
title: Categorical Programming: Morphisms
date: September 9, 2012
---

#### Morphisms

Morphisms are the central idea of category theory. They let us
express mappings between objects and more importantly let us
discuss how compositions of morphisms relate to each other.

The term homomorphism is often used to describe morphisms in a
category since homomorphism describes a structure preserving
property that is associative. This definition is baked into the
category definition and thus sometime we refer to the set of
morphisms in a category as the **hom-set** or $ \\text\{Hom\}_C $

There are some special cases of morphisms.

> **Endomorphism**: $ f : A \\rightarrow A $

An endomorphism with equal domain and codomain.

> **Epimorphism**

Is a morphism $ f : A \\rightarrow B $ such that for any arbitrary
morphisms $ g_1 : B \\rightarrow C $, $ g_2 : B \\rightarrow C $.

Such that equality of left composition implies equality of $ g_1,
g_2 $. i.e.,

$ g_1 . f = g_2 . f \\implies g_1 = g_2 $.

> **Monomorphism**

A monomorphism is similar except with right composition.

$ f . g_1  = f . g_2  \\implies g_1 = g_2 $.

![Illustration](/images/epic_monic.svg).

> **Isomorphism**: $ f : A \\rightarrow B $ if there exists another
morphism $ f^\{-1\} $ such that $ f . f^\{-1\} = \\text\{id\}\_A $
and $ f^{-1} . f = \\text\{id\}\_B $

Isomorphisms are extremely important in category theory since
they represent a form of "structural equality" between objects.
Objects in the domain can be related to objects in the domain
through the isomorphism.

#### Diagrams

This leads to the next discussion of commutative diagrams. A
commutative diagram is a diagrammatic way of expressing the
equivalence of morphisms. We draw objects as labels and morphisms
as arrows between labels. If one "chases" a path through the
diagram and finds that all variations of paths lead to the same
endpoint we say that the diagram **commutes**, meaning the
compositions of morphisms converge at the same codomain
irregardless of choice of composition.

> Because of the ubiquity of diagrammatic reasoning in category theory it
> is common to speak of morphisms and **arrows** interchangeably.

A very common phrase is that a property holds *such that the following
diagram commutes*, meaning that the commutativity of the diagram is
required for some proposition to be true. This allows us to express a
large amount of information about required properties in a very compact
way.

For example the definition of isomorphism could be written such
that the following diagram commutes. The dotted line indicating
the required inverse of $ f $ required to exist by the definition.

![Illustration](/images/iso.svg).
