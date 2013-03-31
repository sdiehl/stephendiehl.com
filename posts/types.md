---
title: Haskell Types for Python Programmers ( Part 1 )  
date: November 19, 2012
---

#### Haskell Types for Python Programmers ( Part 1 )

This is a three part blog about how to think about some of the core
ideas of Haskell in the everyday language of Python of programmers.

- Part 1. Type signatures and type classes
- Part 2. Parametric polymorphism and ad-hoc polymorphism vs subtype polymorphism

#### Introduction

In Haskell all functions have two definitions.

A **value level** definition:

```haskell
and p q = p && q 
```

And a **type level** definition, often called the type signature:

```haskell
and :: Bool -> Bool -> Bool
```

A type signature may contain concrete types such as 

```haskell
Bool
Int
Double
Float
Char
String
```

Or free paramaters ( type variables ) which hold the place for concrete
types. These are most often lower case letters starting at the beginning
of the alphabet in the order they appear in the signature.

```haskell
a
b
x0
x1
```

A signature ``a -> a`` is a function of one argument returning a
value of the same type as the argument.

```haskell
addOne x = x+1
``` 

A rough equivelant in Python would be.

```python
def addOne(x):
    return x+1
```

A signature ``a -> a -> a`` is a function of two arguments of the same
type, returning a value of the same as both the arguments.

```haskell
add x y = x+y
```

```python
def add(x,y):
    return x+y
```

In general the ``(->)`` operation is right associative, i.e. (
``a -> b -> c = a -> ( b -> c )``.

#### Currying

In Python functions arguments can be represented by tuples. For
example the add function:

```python
>>> add = lambda x,y: x+y
>>> args = (1,2)
>>> add(*args)
```

If we were to split the tuple of arguments into two lambda expression
we'd have a function which allowed application of arguments one at a
time:

```python
>>> add = lambda x: lambda y: x+y
>>> addOne = add(1)
<function <lambda> at 0x7fe2ba959b90>
>>> addOne(2)
3
```

Evaluation in Haskell is quite different from Python in that arguments
are explicitly not tuples. Unless explictly indicated Haskell functions
are curried functions each of a single argument.

The standard library also defines two important functions for coercing
between single arguments of tuples and curried functions.

```haskell
curry :: ((a,b)->c) -> a->b->c
curry f a b = f (a,b)
 
uncurry :: (a->b->c) -> ((a,b)->c)
uncurry f (a,b)= f a b
```

For example:

```haskell
-- Single tuple argument 
fst (a,b) = a
-- Multiple argument
root a b = (b + sqrt(b^2-4))/(2*a)
```

```python
# (a,b) -> a
def fst(tup):
    a,b = tup
    return a

# Int -> Int -> Int
def root(a, b):
    return (b + sqrt(b^2-4))/(2*a)
```

```haskell
curry fst :: c -> b -> c
uncurry root :: (c, c) -> c
```

```python
# a -> b -> a
def curry_fst(a):
    def _curry_fst(b):
        return a
    return _curry_fst

# Int -> Int -> Int
def curry_root(a):
    def _curry_root(b):
        return (b + sqrt(b^2-4))/(2*a)
    return _curry_root


# (Int, Int) -> Int
def uncurry_root(tup):
    a,b = tup
    return (b + sqrt(b^2-4))/(2*a)
```

#### Constraints

```haskell
add :: a -> a
addOne x = x+1
```

Notice in both the Haskell and the Python examples above we wrote the
body of function as an addition but nowhere in our type signature did
we specify that our input values need be numeric quantities capable of
being added. The task is known as **constrained genericity**.

Both languages have to handle the case that addition is not defined
for non-numeric quantities. In Python there is fundamentally no type
distinction between numeric quantities and objects which overload
numeric methods such as ``__add__`` magic method.

In Haskell however, the compiler will keep the free parameter preserving
the polymorphism we want, but will attach a constraint on the free
parameter ``a``.

```haskell
addOne :: Num a => a -> a
```

This would read as "A function which takes value of type ``a`` and returns
a value of the same type and the input type is an instance of the
``Num`` typeclass".

So, what is a typeclass you ask? Simply put it is a set of
functions or operators which are implemented over a type.

The **class definition** defines the abstract signature of all instances
of the type class. The following would read "Num defines an
operation which over a type a which takes two arguments also of
type a and returns the same type a".

```haskell
class Num a where
    (+) :: a -> a -> a
```

If we dig into the Haskell standard library, sure enough we find
a type class definition which defines how to add two Int objects.

```haskell
instance  Num Int  where
    (+) x y = plusInt x y
```

The ``plusInt`` function is a C function which performs unboxed integer
multiplication and returns a boxed integer.


Python would "handle" this problem by attempting to call ``__add__``
or ``__radd__`` on the operands, and if methods are not defined
throws an Exception at **runtime**.

```python
class Foo(object):
    def __init__(self, x):
        self.val = x
    
>>> Foo(1) + Foo(1)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unsupported operand type(s) for +: 'object' and 'object'
```

Haskell rejects this at **compile time**.

```haskell
-- fail.hs
data Foo = Foo
main = addOne Foo
```

```bash
$ ghc fail.hs
[1 of 1] Compiling Main             ( fail.hs, fail.o )

    No instance for (Num Foo)
      arising from a use of `addOne'
    Possible fix: add an instance declaration for (Num Foo)
```

In short, Python's notion of duck typing and **ad-hoc polymorphism**
with constraints in Haskell are quite similar from a structural
standpoint, but with the exception that Python is not able to reason
about types in question and defers failures to runtime since it is not
able to reason about the types of arguments while Haskell is able to
reject a large class of invalid programs at compile time.

To remedy the Python program, we make the Foo class quack like a
integer.

```python
class Foo(object):
    def __init__(self, x):
        self.val = x
        
    def __add__(self, other):
        return Foo(self.x + other.x)

>>> Foo(1) + Foo(1)
Foo(2)
```

In Haskell, we define a ``Num`` instance declaration.

```haskell

data Foo a = Foo a

instance Num Foo where
    (Foo x) + (Foo y) = Foo (x + y)

>>> Foo 1 + Foo 1
Foo 2
```

#### Higher Functions

It could be argued that core idea of functional programming is
composition[^2]. Namely the composition of higher functions, and in typed
functional languages; the constrained composition of functions.

 For types for to be useful for functional progrmaming we require that
we be able to compose functions with other functions while maintaining
expressivity and type safety. For example we would like to write
down a generic function which takes a "object with some
structure" and apply a function over the internal structure.

```python
def map(f, xs):
    return (f(x) for x in xs)
```

If we expand the iterator sugar out a bit we find more
illuminating definition:

```python
# (object -> object) -> iterable -> iterable
def map(f, xs):
    return iter(f(x) for x in xs.__iter__())
```

Notice that in Python we have two qualitatively different classes of
"things" in the signature: objects and iterables of objects.

If we mentally translate our word "iterable" to "functor"[^3] in Haskell we
might describe the input function as ``a -> b`` over types ``a`` and
``b`` over a functor on a ( ``f a`` ) to a functor on b (`` f b `` ).

In this case we have two categories of types in the signature a's
and b's. When multiple type variables occur in the signature we
call them ``rigid``. Type variables that are not rigid are free.

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

The type ``f a`` represents a type constructor of with one type
parameter a. The signature of the type we call the **kind**. The kind of
the a simple type is ``*``. The kind of a unary type is ``* -> *`` or a
type over a type, for example the "type of list of type integers".

```haskell
λ: :kind Int
Int :: * 

λ: :kind []
[] :: * -> *
```

For example let's inspect some simple type instances in the GHCI
shell and the magic commands ``:kind`` and ``:type``.

```haskell
λ: data List a = Nil | Cons a
λ: :kind List
List :: * -> *

λ: data Either a b = Left a | Right b
Either :: * -> * -> *
```

```haskell
-- Notice the free parameters
λ: :type Left True
Left False :: Either Bool b

λ: :type Right 3
Left False :: Either a Int
```

#### 

Of course we still want to preserve polymorphism over parameters,
so we might ask how do we spell out maps over structures without
having to define each and every instance of lists. In this case,
in our definition we only specialize one of the type parameters,
namely ``f`` to be the List type.

```haskell

instance Functor List where

    fmap :: (a -> b) -> List a -> List b

    fmap f (Cons x) = Cons (f x)
    fmap f Nil = Nil
```

The type in the constructor of a is still rigid within the functor
defintion. Therefore for lists of all types ( including lists of lists )
we get fmap definitions for free, simply by substitution of one of the
rigid type variables in our parametric type signature. 

#### Further Reading:

* [Subtypes vs. Where Clauses: Constraining Parametric Polymorphism](http://www.cs.cornell.edu/andru/papers/where-clauses.pdf)
* [Haskell for C Programmers](http://www.haskell.org/haskellwiki/Haskell_Tutorial_for_C_Programmers)
* [Making Our Own Types and Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)

[^2]: Along with the rejection of mutable state and implicit effects.

[^3]: The name comes from category theory where functors are
mappings between categories that preserve aspects of the
[specific notions of structure of the category](http://ncatlab.org/nlab/show/functor).
