---
title: Categorical Programming: Functors
date: September 10, 2012
---

#### Functors

A functor is a collection of mappings between categories that
maps morphisms to morphisms and objects two objects. A functor
between two categories $ \\mathcal{C} , \\mathcal{D} $.

$$
T: \mathcal{C} \rightarrow \mathcal{D}
$$

$$
\begin{align}
T &: \ob{\mathcal{C}} \rightarrow \ob{\mathcal{D}} \\
T &: \hom{\mathcal{C}} \rightarrow \hom{\mathcal{D}}
\end{align}
$$

satisfying several laws:

$$
\begin{align}
T(\id{A})    &= \id{T(A)} \\
T(f \circ g) &= T(f) \circ T(g)
\end{align}
$$

In functional programming we differentiate between the **functorial
action** or **functorial image** on an object ( ``` T a ``` ) from the
functorial action on a Haskell function is denoted ``` fmap f ```. In
the math notation these are both written as $ T(x) $ and $ T(f) $.

![Illustration](/images/functors2.svg).

![Illustration](/images/functors.svg).

![Illustration](/images/functors3.svg).

#### Functor

In **Hask** the Functor typeclass is defined as:

```haskell
class Functor t where
  fmap :: (a -> b) -> t a -> t b
```

The functor laws manifest as following identities:

```haskell
fmap (g . f) = fmap g . fmap f
fmap id = id
```

The canonical example is the list functor which lifts a function
into the ordered elements of a list.

```haskell
instance Functor [] where
    fmap f []       =  []
    fmap f (x:xs)   =  f x : fmap f xs
```

We see the the list functor is a mapping between **Hask** type ``a``
and the list parameterized by the type ``a``. The functor maps values
of type ``a`` to ordered collections of values of type ``a`` namely
``[a]``. As well as functions over types ``a -> b`` to ordered
collections of type ``b`` namely ``fmap f``.

The constructor ``[]`` is an example of an **endofunctor** over
**Hask**.

```
[] :: * -> *
[] :: [a]
```

Another classic example is Haskell functor over ``Pair`` values and
the pair functor.

```haskell
data Pair a b = Pair a b

instance Functor Pair where
    fmap f (Pair x y) = Pair (x) (f y)

```

In the first article we defined the trivial category **One**
which also supports a functor instance.

```haskell
data One a b where
    Ida :: One a ()

instance Category One where
    id = Ida
    Ida . Ida = Ida

instance Functor One where
  fmap f (Ida a _) = Ida (f a) ()
```

A more mathematical example is the powerset defined as the set of
all subsets over a set $ A $. The functor is defined as the
powerset operator, written $ 2^A $ as well as a morphism mapping
that lifts $ f $ over

$$
\begin{align}
P(A) &= \{ a | a \subseteq A \} \\
P(f) &= \{ f(a) | a \in A \}
\end{align}
$$

#### Applicative Functors

Maps types of results into types of effectful computations. (``IO a``)

Maps functions into functions that chain the results of effectful
computations ``fmap f``.

```haskell
class Functor f => Applicative f where
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b
```

```haskell
pure id <*> v = v
pure f <*> pure x = pure (f x)
```

```haskell
instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)
```

![Applicative](/images/applicative.svg).

![Applicative](/images/applicative2.svg).

![Applicative](/images/applicative3.svg).

```haskell
let res = f <$> a <*> b <*> c where 
    a = readLn :: IO String
    b = readLn :: IO String
    c = readLn :: IO String
```
