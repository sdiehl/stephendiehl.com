---
title: Categorical Programming: Functors
date: September 10, 2012
---

#### Functors

A functor is a collection of mappings between categories that map
between the the morphism and objects of two categories. More precisely
it is a mapping between the categories $ T: A \\rightarrow B $ such that
for every object in $ X \\in A $ there exists

$$
T: X \rightarrow T(X) \in B
$$

And for each morphism $ f : X \\rightarrow Y \\in \\text{Hom}_A $

$$
T(f): T(X) \rightarrow T(Y) \in \text{Hom}_B
$$

In functional programming we differentiate between the **functorial
action** or **functorial image** on an object ( ``` T a ``` ) from the
functorial action on a morphism usually denoted ``` fmap f ```.

In **Hask** the Functor typeclass is defined with:

```haskell
class Functor t where
  fmap :: (a -> b) -> t a -> t b
```

![Illustration](/images/functor.svg).

#### Functor laws

The essential nature of Functors is that we preserve composition
structure under mapping. Namely:

$$
T ( g . f ) = (T g) . (T f)
$$

```haskell
fmap (g . f) = fmap g . fmap f
```

```haskell
fmap id = id
```

#### Hask

The classic example is Haskell functor over ``Pair`` values and
the list functor.

```haskell
data Pair a b = Pair a b

instance Functor Pair where
    fmap f (Pair x y) = Pair (x) (f y)

instance Functor [] where
    fmap f []       =  []
    fmap f (x:xs)   =  f x : fmap f xs
```

