---
title: Navigating the Postmodern Python World
date: July 10, 2013
---

## Navigating the Postmodern Python World

If there is one defining feature of modern Python, it's simply
that is that the definition of Python itself is becoming
increasingly blurred. Many projects over the last few years have
taken larger and larger leaps to extend Python and reconstruct
what "Python" itself means.

At the same time on there are variety of technologies encroaching
on Python's niche, each bringing a variety of advantages:

1. Go - ( Goroutines, Types, Interfaces )
1. Rust - ( Traits, Speed, Types )
1. Julia - ( Speed, Types, Multiple Dispatch )
1. Scala - ( Traits, Speed, Types )
1. Clojure ( Metaprogramming, DSLs, Protocols )

This is a short field-guide to the various Python responses to
these technologies and new libraries and models:

Metaprogramming
---------------

MacroPy is a metaprogramming framework which extends Python AST
with a variety of syntatic constructs for modern language
features which compile into standard Python code. For example we
can achieve a measure algebraic datatypes using 

```python
from macropy.case_classes import case

@case
class Nil():
    pass

@case
class Cons(x, xs):
    pass

Cons(1, Cons(2, Cons(3, Nil())))
```

And then pattern match on those declared types:

```python
def reduce(op, my_list):
    with switch(my_list):
        if Cons(x, Nil()):
            return x
        elif Cons(x, xs):
            return op(x, reduce(op, xs))
```

What is missing however is a extensible staged metaprogramming
system along the lines of
[camlp4](http://caml.inria.fr/pub/docs/tutorial-camlp4/). This is
however being addressed in the [Mython
](https://github.com/mythonlang) which provides a parser
framework [pgen2](https://github.com/mythonlang/pgen2) for
defining new syntax for quoted blocks.

```python
my[namedtupledef] Point(x, y): pass

my[c]:
    int add (int x, int y) {
        return x + y;
    }

print "Regular Python"
```

Types
-----

Python is dynamically typed language and proud of it. While I
certainly don't wish to fan the flames of the typing holy-war,
there is certainly a large school of thought that building
reliable applications requires stronger guarantees than unit
testing alone can provide. Type systems as defined by Benjamin
Pierce are:

> ... tractable syntactic method for **proving** the absence of
> certain program behaviors by classifying phrases according to
> the kinds of values they compute.

The emphasis is on proving properties about the space of all
program behavior instead of simply enumerating a finite number of
cases.  It's doubtful that going full-static typing on Python is
the right solution, however there are certainly better
compromises that can be reached between excess dynamic dispatch
and static guarantees. The [MyPy
project](http://www.mypy-lang.org/) has reached a nice
equilibrium allowing both typed and untyped code to exist in a
superset of the language. For example:

```python
def simple_typed(x : int, y : int) -> int:
    return x + y

simple_typed(1, 2)     # Type-checks succesfully

# Fails: Argument 2 to "simple_typed" has incompatible type # "float"
simple_typed(1, 2.0)

# Fails: Argument 2 to "simple_typed" has incompatible type "str"
simple_typed(1, "foo")
```

Of course that isn't much more useful than C. So of course we
aren't limited to just simply typed functions, there are also
variables types like generics, arrow types and a variety of built
in type-level functions.

```python
from typing import Iterator, typevar, Generic, Function, List

T = typevar('T')

def example_typed(x : Iterator[int]) -> Iterator[str]:
    for i in x:
        yield str(i)

def example_generic(x : Iterator[T]) -> Iterator[T]:
    for i in x:
        yield i
```

We can also model more advanced generic structures like Functors
and Monads.

```python
a = typevar('a')
b = typevar('b')

class Functor(Generic[a]):
    def __init__(self, xs : List[a]) -> None:
        self._storage = xs

    def iter(self) -> Iterator[a]:
        return iter(self._storage)


def fmap(f : Function[[a], b], xs : Functor[a]) -> Functor[b]:
    return Functor([f(x) for x in xs.iter()])



class Monad(Generic[a]):
    def __init__(self, val : a) -> None:
        self.val = val


class IdMonad(Monad):

    # Monad m => a -> m a
    def unit(self, x : a) -> Monad[b]:
        return IdMonad(x)

    # Monad m => m a -> (a -> m b) -> m b
    def bind(self, x : Monad[a], f : Function[[a], Monad[b]]) -> Monad[b]:
        return f(x.val)

    # Monad m => m (m a) -> m a
    def join(self, x : Monad[Monad[a]]) -> Monad[a]:
        return x.val
```

Speed
-----

The most important recent development in "higher-performance"
Python is the development of the higher level DataFrame container
provided by the [Pandas
library](http://pandas.pydata.org/pandas-docs/dev/index.html).
Pandas operates in a very hybrid-Python space, utilizing NumPy
for some operations, Cython for others, and even vanilla C for
some it's internal hash tables. Panda's undogmatic approach
to the it's underlying architecture has lead to it becoming
the de-facto library for data analysis. Pandas development
embodies much of what makes the numeric Python ecosystem
great.

```python
In [1]: from pandas import DataFrame

In [2]: titanic = DataFrame.from_csv('titanic.csv')

In [3]: titanic.groupby('pclass').survived.mean()
pclass
1st       0.619195
2nd       0.429603
3rd       0.255289
Name: survived
```

However the latest incarnation in attempts to improve Python
performance has been to utilize the LLVM compiler infrastructure
to selectively compile segments of Python code into native code.
While there is currently a shootout between the various
technologies doing this, most of them do something like the
following:

1. Some form of ``@jit`` or ``@compile`` decorator is attached
   to a function.
1. The AST or bytecode of the function is extracted and fed into
   the compiler pipeline where it is mapped into an internal AST
   determining how the given logic in the function lowers to
   machine code given a certain set of input types.
1. The compiled function is called with a set of types, the
   arguments are inspected and code is generated given the
   arrangements of types. The resulting code is cached against
   arguments so that subsequent calls dispatch straight to the
   native code.

These projects have resulted in increased interest in Python
language technologies and the development of the
[``llvmpy``](https://github.com/llvmpy/llvmpy) project which I
suspect will be more significant to the history of Python than
any specific JIT compiler.

The simplest example of usage ( taken from the wonderful
[Kaleidescope](http://llvm.org/docs/tutorial/LangImpl1.html)
tutorial ) is a to create a simple native multiply-add function
and then call it by unboxing three Python integers:

```python
import llvm.core as lc
import llvm.ee as le

mod = lc.Module.new('mymodule')

i32 = lc.Type.int(32)
funty = lc.Type.function(lc.Type.int(), [i32, i32, i32])

madd = lc.Function.new(mod, funty, "multiply")
x = madd.args[0]
y = madd.args[1]
z = madd.args[2]

block = madd.append_basic_block("L1")

builder = lc.Builder.new(block)
x0 = builder.mul(x, y)
x1 = builder.add(x0, z)

builder.ret(x1)

print mod

tm = le.TargetMachine.new(features='', cm=le.CM_JITDEFAULT)
eb = le.EngineBuilder.new(mod)
engine = eb.create(tm)

ax = le.GenericValue.int(i32, 1024)
ay = le.GenericValue.int(i32, 1024)
az = le.GenericValue.int(i32, 1024)

ret = engine.run_function(madd, [ax, ay, az])

print ret.as_int()
print mod.to_native_assembly()
```

Which compiles generates the following LLVM IR.

```python
define i32 @multiply(i32, i32, i32) {
L1:
  %3 = mul i32 %0, %1
  %4 = add i32 %3, %2
  ret i32 %4
}
```

While this example is a bit contrived it is however possible to
generate very fast JIT'd functions that integrate well with
libraries like NumPy that store their data as large blocks of
unboxed memory.

Interfaces
----------

The task of decomposing behavior into composable units instead of
going explicit inheritance hierarchies is a problem that Python
arguably does not solve particular well and can often lead to
nightmarishly complicated use of mixins. However, some sanity can
be regained by using ABC module to emulate and statically defined
interfaces.

```python
import heapq
import collections

class Heap(collections.Sized):
   def __init__(self, initial=None, key=lambda x:x):
       self.key = key
       if initial:
           self._data = [(key(item), item) for item in initial]
           heapq.heapify(self._data)
       else:
           self._data = []

   def pop(self):
       return heapq.heappop(self._data)[1]

   def push(self, item):
       heapq.heappush(self._data, (self.key(item), item))

   def len(self):
       return len(self._data)
```

For example to build an equivalence class that spans all
instances of classes that implement an ``eq()`` method, we might
do the following:

```python
from abc import ABCMeta, abstractmethod

class Eq(object):

    __metaclass__ = ABCMeta

    @classmethod
    def __subclasshook__(cls, C):
        if cls is Eq:
            for B in C.__mro__:
                if "eq" in B.__dict__:
                    if B.__dict__["eq"]:
                        return True
                    break
        return NotImplemented

def eq(a, b):
    if isinstance(a, Eq) and isinstance(b, Eq) and type(a) == type(b):
        return a.eq(b)
    else:
        raise NotImplementedError

class Foo(object):
    def eq(self, other):
        return True

class Fizz(Foo):
    pass

class Bar(object):
    def __init__(self, val):
        self.val = val

    def eq(self, other):
        return self.val == other.val

print eq(Foo(), Foo())
print eq(Bar(1), Bar(1))
print eq(Foo(), Bar(1))
print eq(Foo(), Fizz())
```

However extending this kind of interface notion to functions of
multiple arguments and becomes increasingly contingent on
querying ``__dict__`` and is brittle under composition. The heart
of the problem is attempting to decompose everything into
interfaces that vary on a single type, when what we really want
is to declare interfaces that span families of multiple types.
This kind of shortcoming in OOP goes straight to the heart of the
[Expression
Problem](http://en.wikipedia.org/wiki/Expression_problem).

Languages like Scala, Haskell and Rust provide solutions to this
problem in the form of traits and typeclasses. For example
Haskell can automatically derive the differentiation equations
for the cross product of all types which implement Floating point
and Equality interfaces.

```haskell
instance (Floating a, Eq a) => Floating (Dif a) where
    pi               = C pi

    exp (C x)        = C (exp x)
    exp (D x x')     = r where r = D (exp x) (x' * r)

    log (C x)        = C (log x)
    log p@(D x x')   = D (log x) (x' / p)

    sqrt (C x)       = C (sqrt x)
    sqrt (D x x')    = r where r = D (sqrt x) (x' / (2 * r))
```

Asynchronous Programming
------------------------

In this case again we have a variety of bolted-on solutions that
address the problem partially but introduce a whole set of
constraints and patterns which go against the grain of regular
Python.  Gevent in particular adds continuations to Python itself
through splicing of the underlying C stack with assembly hacks.
The resulting API is very elegant API but makes reasoning about
control-flow and exceptions much more complex.

```python
import gevent

def foo():
    print('Running in foo')
    gevent.sleep(0)
    print('Explicit context switch to foo again')

def bar():
    print('Explicit context to bar')
    gevent.sleep(0)
    print('Implicit context switch back to bar')

gevent.joinall([
    gevent.spawn(foo),
    gevent.spawn(bar),
])
```

The control flow is show below:

![](http://sdiehl.github.io/gevent-tutorial/flow.gif)

Through rather ungraceful monkey-patching of the standard library
we can emulate Erlang-style actor behavior with asynchronous
entry points and internal state: 

```python
import gevent
from gevent.queue import Queue
from SimpleXMLRPCServer import SimpleXMLRPCServer

class Actor(object):
    _export = [
        'push',
    ]

    def __init__(self, address):
        self.queue = Queue()

        self._serv = SimpleXMLRPCServer(address, allow_none=True, logRequests=False)
        self.address = address

        for name in self._exports:
            self._serv.register_function(getattr(self, name))

    def push(self, thing):
        self.queue.put(thing)

    def poll(self):
        while True:
            print(self.queue.get())

    def periodic(self):
        while True:
            print('PING')
            gevent.sleep(5)

    def serve_forever(self):
        gevent.spawn(self.ping)
        gevent.spawn(self.get)
        self._serv.serve_forever()

def main():
    from gevent.monkey import patch_all
    patch_all()

    serve = Actor(('', 8000))
    serve.serve_forever()
```

DSLs
----

The [Z3 project](http://rise4fun.com/z3py/tutorial) is a
embedding of an external API on top of the Python object layer.
For instance in Z3 to solve the n-queens problem the problem is
described with Python expressions and solved by the external SMT
solver:

```python
from Z3 import *

Q = [ Int('Q_%i' % (i + 1)) for i in range(8) ]

# Each queen is in a column {1, ... 8 }
val_c = [ And(1 <= Q[i], Q[i] <= 8) for i in range(8) ]

# At most one queen per column
col_c = [ Distinct(Q) ]

# Diagonal constraint
diag_c = [ If(i == j, 
              True, 
              And(Q[i] - Q[j] != i - j, Q[i] - Q[j] != j - i)) 
           for i in range(8) for j in range(i) ]

solve(val_c + col_c + diag_c)
```

Several other projects from Theano, SymPy to PySpark make
extensive use of the overloading of operators to layer
domain-specific semantics on top of Python expressions.

```python
from sympy import Symbol
from sympy.logic.inference import satisfiable

x = Symbol('x')
y = Symbol('y')
satisfiable((x | y) & (x | ~y) & (~x | y))
```
