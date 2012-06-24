---
title: Categorical Programming: Functors
date: September 10, 2012
---

#### Functor

A functor is a pair of mappings between categories that maps
morphisms and objects between the two categories. More precisely
it is a mapping between $ t: A \\rightarrow B $ such that for
every object in A there exists

$$
t: A \\rightarrow t(A) \\in B
$$

And for each morphism $ f $

$$
t(f): t(X) \\rightarrow t(Y) \\in B
$$

In functional programming we differentiate between the functorial
action on objects  ( ``` t a ``` )  from the functorial action on
morphisms usually denoted ``` fmap f ```.

In **Hask** the Functor typeclass is defined with:

```haskell
class Functor t where
  fmap :: (a -> b) -> t a -> t b
```

![Illustration](/images/functor.svg).

#### Functor laws

The essential nature of Functors is that we preserve composition
struction under mapping. Namely:

$$
t(g . f ) = (t g) . (t f)
$$

```haskell
fmap (g . f) = fmap g . fmap f
```

\

Or pictorally:

![Illustration](/images/functor_comp.svg).

The previous law also implictly depends on the preservation of
identities, indeed the identity morphism for an object must be preserved
by functorial action.

```haskell
fmap id = id
```

Or pictorially: 
\

![Illustration](/images/functor_id.svg).

#### Hask

The classic example is Haskell functor over ``Pair`` values and
the list functor.

```haskell
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Functor [] where
    fmap f []       =  []
    fmap f (x:xs)   =  f x : fmap f xs
```

\

A rough illustration of the List functor pictorially.

![Illustration](/images/fmap.svg).

#### Contravariant Functors

A slight variant on the traditional functor is a **contravariant
functor** which reverses the domain and codomain of morphisms.

$$
t: A \\rightarrow t(A) \\in B
$$

And for each morphism $ f $

$$
t(f): t(Y) \\rightarrow t(X) \\in B
$$

![Illustration](/images/confunctor.svg).

Traditional functors as discussed above can also be referred to
**covariant functors**.
