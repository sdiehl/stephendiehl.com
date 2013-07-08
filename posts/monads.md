---
title: Monads made difficult
date: July 7, 2013
---

### Monads Made Difficult

This is a short, fast and analogy-free introduction to Haskell
monads derived from a categorical perspective. This assumes you
are familiar with Haskell typeclasses and basic category theory.

In the beginning there is a category $\mathcal{C}$ with objects
and morphisms.

* Objects   : $●$
* Morphisms : $● \rightarrow ●$

For each object there is an identity morphism ``id`` and a
composition rule $(\circ)$ for combining morphisms associatively.

```haskell
class Category c where
    id  :: c x x
    (.) :: c y z -> c x y -> c x z
```

In Haskell there is a category called *Hask*.

```haskell
type Hask = (->)
 
instance Category Hask where
    id x = x
    (g . f) x = g (f x)
```

***

Between two categories we construct a *functor* ($T$), which maps
between objects and morphisms of categories.

* Objects   : $T(●)$
* Morphisms : $T (● \rightarrow ●)$

```haskell
class (Category c, Category d) => Functor c d f where
    fmap :: c a b -> d (f a) (f b)
```

The identity functor $1_\mathcal{C}$ for a category $\mathcal{C}$
is functor mapping all objects to themselves and all morphisms to
themselves.

An *endofunctor* is a functor from a category to itself.

```haskell
type Endofunctor c f = Functor c c f
```

The repeated image of a endofunctor over a category is written with
exponential notation:

$$
\begin{align*}
T^2 &= T T : \mathcal{C} \rightarrow \mathcal{C} \\
T^3 &= T T T: \mathcal{C} \rightarrow \mathcal{C}
\end{align*}
$$

***

For two functors $F,G$ between two categories $\mathcal{A,B}$:

$$
F : \mathcal{A} \rightarrow \mathcal{B} \\
G : \mathcal{A} \rightarrow \mathcal{B}
$$

We construct a *natural transformation* $\eta$ between functors
$\eta : F \rightarrow G$ that associates every object in $X$ in
$\mathcal{A}$ to a morphism in $\mathcal{B}$:

$$
\eta_X : F(X) \rightarrow G(X)
$$

<p>
<img src="/images/nat.svg" width="150px"/>
</p>

Such that the following *naturality condition* holds for any
moprhism $f : X \rightarrow Y$:

$$
\eta_Y \circ F(f) = G(f) \circ \eta_X
$$

<p>
<img src="/images/naturality.svg" width="200px"/>
</p>

Expressible in our general category class as the following type
alias, and in Hask as a family of polymorphic functions with
signature:

```haskell
type Nat k f g = forall a. k (f a) (g a)
```

```haskell
type Nat Hask f g = forall a. f a -> g a
```

***

We finally can define our *monad* over a category $\mathcal{C}$
to be a triple of:

* An endofunctor $T: \mathcal{C} \rightarrow \mathcal{C}$
* A natural transformation $\eta : 1_\mathcal{C} \rightarrow T$
* A natural transformation $\mu : \mathcal{T}^2 \rightarrow T$

```haskell
class (Endofunctor c t) => Monad c t where
    eta :: c a (t a)
    mu  :: c (t (t a)) (t a)
```

With several *coherence conditions* commonly called the *monad laws*.

$$
\mu \circ T \mu = \mu \circ \mu T \\
$$

<p>
<img src="/images/coherence1.svg" width="200px"/>
</p>

$$
\mu \circ T \eta = \mu \circ \eta T = 1_T \\
$$

<p>
<img src="/images/coherence2.svg" width="200px"/>
</p>

Or triple can be defined as a series of *string diagrams*.

<p>
<img src="/images/string3.svg"/>
</p>

With the coherence conditions given diagrammatically:

<p>
<img src="/images/string1.svg"/>
</p>

<p>
<img src="/images/string2.svg"/>
</p>

***

In Haskell we define a bind ``(>>=)`` operator defined in terms
of the natural transformations and ``fmap`` of the underlying
functor. The ``join`` and ``return`` functions are also just
aliases for the two natural transformations.

```haskell
(>>=) :: (Monad c t) => c a (t b) -> c (t a) (t b)
(>>=) f = mu . fmap f
 
return = eta
join = mu
```

The equivalent coherence conditions expressed in terms of bind
and return are:

```haskell
return a >>= f == f a
m >>= return == m
(m >>= f) >>= g == m >>= (\x -> f x >>= g)
```

Or equivalently in do notation:

```haskell
m :: t a
f :: a -> t b
g :: b -> t c
```

```haskell
do x <- m
   return x

= do m

do y <- return x
   f y

= do f x
```

```haskell
  do b <- do a <- m
             f a
     g b

= do a <- m
     b <- f a
     g b

= do a <- m
     do b <- f a
        g b
```

***

In the case of Hask where ``c = (->)`` then we indeed see an
equivelant derivation of Monad and Functor similar to the prelude
( if the Prelude had the proper Functor/Monad hierarchy! ).

```haskell
class Functor t where
    fmap :: (a -> b) -> t a -> t b

class (Functor t) => Monad t where
  eta :: a -> (t a)
  mu  :: t (t a) -> (t a)

  (>>=) :: t a -> (a -> t b) -> t b
  ma >>= f = join . fmap f
```

The List monad would have the implementation:

* $\eta$ returns a singleton list from a single element.
* $\mu$ turns a nested list into a flat list.
* $\mathtt{fmap}$ applies a function over the elements of a
  list.

```haskell
instance Functor [] where
    fmap f (x:xs) = f x : fmap f xs

instance Monad [] where
  eta :: a -> [a]
  eta x = [x]

  mu :: [[a]] -> [a]
  mu = concat
```

The IO monad would have the intuition of implementation:

* $\eta$ returns a pure value usable within the context of the
  computation.
* $\mu$ turns a sequence of IO operation into a single
  IO operation.
* $\mathtt{fmap}$ applies a function over the result
  of the computation.
