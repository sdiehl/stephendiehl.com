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
category definition and thus the convention of referring to the
morphisms in a category as the **homset** or $ \\text\{Hom\}_C $

There are some special cases of morphisms.

Morphisms    
------------  ---                              ---
Endomorphism  $$ A \in C $$                    $$ f : A \rightarrow A $$
Epimorphism   $$ g_1, g_2 \in \text{Hom}_C $$  $$  f . g_1  = f . g_2 \implies g_1 = g_2 $$
Monomorphism  $$ g_1, g_2 \in \text{Hom}_C $$  $$  g_1 . f = g_2 . f  \implies g_1 = g_2 $$

> **Endomorphism**

An endomorphism is a morphism with equal domain and codomain.

> **Epimorphism**

Is a morphism $ f : A \\rightarrow B $ such that for any arbitrary
morphisms:

$$ 
g_1 : B \rightarrow C \\
g_2 : B \rightarrow C
$$

The equality of left composition implies equality of $ g_1,
g_2 $:

$$ g_1 . f = g_2 . f \implies g_1 = g_2 $$

> **Monomorphism**

A monomorphism is similar except with right composition. With:

$$ 
f : B \rightarrow C \\
g_1 : A \rightarrow B \\
g_2 : A \rightarrow B
$$

$$ f . g_1 = f . g_2 \implies g_1 = g_2 $$

> **Isomorphism**

$ f : A \\rightarrow B $ is an isomorphism if there exists another
morphism $ f^\{-1\} $ such that 

$$ 
    f^{-1} . f = \text{id}_A \\
    f . f^{-1} = \text{id}_B 
$$

We say two objects are isomorphic if there exists an isomorphism
between them,  written as $$ A \cong B $$.

#### Morphisms in Set

In SET morphisms are functions between sets. The notion
monomorphism can be thought of as a generalization of
injective or one-to-one functions. Recall that injectivity
refers to the property that for two elements $ x_1, x_2 $ in a
set $ S $ we have:

$$
    \forall x_1, x_2 \in S: f(x_1) = f(x_2) \implies x_1 = x_2
$$

If you stare closely at the notion of monomorphim and injectivity it
becomes clear that the monomorphism is a **pointless** analogue of
injectivity. Pointless or **point-free** meaning that it excludes the
notion of a elements and expresses the same thought purely in terms
of categories and morphisms. 

$$
    \forall g_1, g_2 \in \text{Hom}_S: f \circ g_1 = f \circ g_2 \implies g_1 = g_2
$$


The same applies for **surjective** ( onto ) functions which are
generalized by epimorphisms, given this new notation let us now refer
( when talking about SET! ) to injective functions as **monic** and
surjective functions as **epic**.

![Illustration](/images/epic_monic.svg).

In SET we call a function which is both epic and monic a **bijection**,
namely this is a function $ f $ which maps all elements to a unique
value and that each element finds a corresponding element in the
codomain under $ f $. In SET it is not difficult to see that these two
combined conditions imply that $ f $ must be invertable ( $
f^{-1} $ ) which is precisely
the definition of isomorphism from above.

Isomorphisms are extremely important in category theory since
they represent a form of "structural equality" between objects.
Namely that through the isomorphism we can move back and forth
between either object and dicuss properties of objects in terms
of their isomorphisms all without the notion of elements!


#### Motivating Example of Isomorphisms

#### Diagrams

This leads to the next discussion of commutative diagrams. A
commutative diagram is a diagrammatic way of expressing the
equivalence of morphisms. We draw objects as labels and morphisms
as arrows between labels. If one "chases" a path through the
diagram and finds that all variations of paths lead to the same
endpoint we say that the diagram **commutes**, meaning the
compositions of morphisms converge at the same codomain
irregardless of choice of composition.

Because of the ubiquity of diagrammatic reasoning in category theory it
is common to speak of morphisms and **arrows** interchangeably.

A very common phrase is that a property holds *such that the following
diagram commutes*, meaning that the commutativity of the diagram is
required for some proposition to be true. This allows us to express a
large amount of information about required properties in a very compact
way.

For example the definition of isomorphism could be written such
that the following diagram commutes. The dotted line indicating
the required inverse of $ f $ required to exist by the definition.

![Illustration](/images/iso.svg).

#### Control.Arrow

Beware, there is a library in Haskell found in ``Control.Arrow`` that can
be somewhat misleading to those studying category theory and
Haskell. The ``Arrows`` package does not model categorical
arrows. Arrows are a more exotic structure known as Freyd
categories.
