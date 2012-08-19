---
title: Categorical Programming: Introduction
date: September 8, 2012
---

### Forward

This is a multipart blog post on the structure of the mathematical
field of Category theory and and how it relates to real world
programming.

Category theory distills the essense of a large variety of constructions
in traditional set theory to more abstract context which can be used to
reason about the large scale structure of mathematics, physics, and
computer science.

If one were to try and describe the essence of category theory, it is
that it is study of relationships between between mappings of elements,
but at a level of generality where there is no notion of individual
elements. Instead elements arise only out of their relationships between
each other.

This tutorial is a humble attempt to provide a bridge between the
mathematical literature and the everyday language of the working Haskell
programming programmer. Any and all feedback is welcome!

### Categories

A *category* is a construction with three things:

1. A collection of **objects**.
2. A collection of **morphisms**.
3. An **composition** operation (.) which has the.
4. For each object A there is an identity morphism $ id_A $.

I will use the following tabular format for the definition of categories
from here out. The first column is the type of construction, the second
column is the constraints on the objects involved in the definition, and
the third column is the definition of the construction.

<table>
<th>Category</th>
<tr>
    <td>Objects</td>
    <td></td>
    <td></td>
</tr>

<tr>
    <td>Morphisms</td>
    <td> $ A,B \\in Hom_C $ </td>
    <td> $ A \\rightarrow B $ </td>
</tr>

<tr>
    <td>Composition</td>
    <td> $ f : B \\rightarrow C $ <br/> $ g : A \\rightarrow B $ </td>
    <td> $ f \\circ g : A \\rightarrow C $ </td>
</tr>

<tr>
    <td>Identities</td>
    <td>For any $ A $ </td>
    <td> $ id_A : A \\rightarrow A $ </td>
</tr>

</table>

The corresponding definition table for a category is that of the
laws for the category. 

**Associativity**

<table>
<th>Set</th>
<tr>
    <td> Associativity </td>
    <td> $ f,g \\in Hom_C $</td>
    <td> $ ( f . g ) . h = f . ( g . h ) $ </td>
</tr>
<tr>
    <td> Identity </td>
    <td> $ A \\in C $</td>
    <td> $ f . id_A  = id_A . f = f $ </td>
</tr>
</table>

It is worth noting the common confusion that morphisms are not
functions. It is the other way around, in the category **SET** functions
are morphisms with objects as sets but this is a a special case. In
general morphisms are a pure abstraction which have structural
similarity to functions.

![Illustration](/images/category1.svg).

#### SET

Categories are often written in bold. For example the category
**SET** is often a motivating topic of discussion since classical
set theoretic definitions are often generalized.

1. Classical sets as objects.
2. Total functions as morphisms.
3. A composition operation $ \\circ $.

<table>
<th>**Set**</th>
<tr>
    <td>Objects</td>
    <td>Set $ C $ </td>
</tr>

<tr>
    <td>Morphisms</td>
    <td>Total functions over $ C $ </td>
</tr>

<tr>
    <td>Composition</td>
    <td> $ f : B \\rightarrow C $ <br/> $ g : A \\rightarrow B $ </td>
    <td> 
    $ f \\circ g : A \\rightarrow C $  <br/>
    $ f \\circ g = \\lambda x. f (g a) $ 
    </td>
</tr>

<tr>
    <td>Identities</td>
    <td>$ A \\in C $</td>
    <td>
    $ id_A :: A \\rightarrow A $ <br/>
    $ id_A = \\lambda x . x $
    </td>
</tr>

</table>


\[
g \\circ f = \\{ (x, g(f(x))) | x \\in A \\}
\]

4. For each set $A$ there is an identity function $ id_A $ which
maps the set $A$ to itself $ f(A) = A $.

With the usual properties:

<table>
<th>**Set**</th>
<tr>
    <td> Associativity </td>
    <td> $ ( f \\circ g ) \\circ h = f \\circ ( g \\circ h ) $ </td>
</tr>

<tr>
    <td> Identities </td>
    <td> $ f \\circ id_A  = id_A \\circ f = f $ </td>
</tr>

</table>

#### HASK

The set of all non-polymorphic Haskell types forms a category
with Haskell functions as morphisms. For example:

```haskell
head :: [a] -> a
head (x:_) =  x
```

\

HASK also contains both a composition operator and a identity the
Prelude.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g =  \ x -> f (g x)
```

```haskell
id :: a -> a
id x =  x
```

\

The constraint on domain and codomain alignment is enforced
by the type checker.

A **subcategory** is a category contained within another category
which also satisfies the category construction. A more advanced
example that will be discussed later is that monads with an
operation called Kleisli composition form a category that is of
much interest to Haskell programmers.

#### VEC

For the linear-algebra inclined, a space of vectors with linear mappings
between vector spaces also forms a category with.

1. Vectors spaces as objects.
2. Linear mappings as morphisms.
3. A composition operation $ \\circ $ composing linear mappings.
4. For each vector $ A $ there is an identity mapping $ id_A $.

#### CAT

In the original definition of category we did not mention that the
objects or morphisms necessarily form a set. Instead we stated in the
definition that we need only have a collection of objects and morphisms.

A category which has its morphisms and objects in a set is referred to as a
**small category**. A category which does not is referred to as a
**large category**.

This begs the question as to whether we can form the category
of all categories. The answer to this is no, because of
the implications of self-referential sets. See [Russell's
Paradox](http://en.wikipedia.org/wiki/Russell's_paradox).

It is however very illuminating to look at the category of all small
categories called **CAT** which does indeed form a category with objects
as categories and mappings called functors as morphisms between
categories.

The category of categories is where category theory starts to
become interesting. More on this in the next post...
