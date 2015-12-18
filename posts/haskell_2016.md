---
title: Reflecting on Haskell in 2015
date: December 23, 2016
---

### Reflecting on Haskell in 2015

It's been quite a year in the language we know and love called Haskell both for
me personally and as a community. Personally I've been fortunate enough to write
quite a bit of Haskell professionally. It's an interesting experience and
changed my perspective quite a bit on the state of our profession.

For the language and community side there's been quite a bit of technical
progress; probably not as much as some of us hope for, but Haskell is not a
language of rapid hype, it's a multi-decade long engineering program.  Avoiding
success at all costs takes time and learning to accept the "long game" has been
something I've come to accept with Haskell. Nevertheless I like to reflect on
what's changed and what I think will matter in 2016.

#### GHC

The Glorious Haskell Compiler 7.10 arrived in March and brought with it several
major new features, as well as an abundance of bug fixes, changes to the backend
compiler, code generation, and linker.

The *The Applicative Monad Proposal* landed finally making Applicative a
superclass of Monad.

The *Burning Bridges Proposal* landed which reexported many of the meromorphic
traversal functions found in the Prelude in favor of their polymorphic
Traversable equivalents. There was much debate.

GHC 8.0 has several large changes that are in the works: 

*[-XOverloadedRecordFields](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)*

The long awaited Overload Recored Fields extension will likely land and address
some of the issues found in the old record system with overloaded selector
names.

For example given the two records with identical accessors.

```haskell
data S = MkS { x :: Int }
  deriving Show

data T = MkT { x :: Bool, y :: Bool -> Bool, tField :: Bool }
```

The following program will typecheck since the accessors are unambiguous at
their call sites.

```haskell
b = s { x = 3 } :: S
c = (t :: T) { x = False }
```

This doesn't quite add first class records to the language yet, so code like the
following polymorphic update of a field "x" can't exist. There are some
interesting proposals on the board about solutions to this though.

```haskell
-- hypothetical
b :: r { x :: Int } -> Int
b r = x r
```

*[-XTypeInType](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell/Phase1)*

The largest change pending is a particularly ambitious program to graft some
features of dependent types onto Haskell. The Kind equalities patch pending in
8.0 at makes the type system (from values up) fully dependent, whereas before we
would have to rely on particularly inelegant hacks.  In GHC 7.x The type of of
the kind ★ used to be the sort ``BOX`` under the old regime.

```haskell
> import GHC.Exts (BOX, Constraint)
> :kind Constraint 
Constraint :: BOX
> :kind BOX
BOX :: BOX
```

Under the new work, ``★`` can be now represented by the identifier ``Type``, and
the type of ``Type`` is itself.

```haskell
Constraint :: *
type (*) = *
type Type = *
```

And more importantly type-level equalities concerning types are reflexive, so a
proof that ``t1 :: k1 ~ t2 :: k2`` now proves that ``t1`` and ``t2`` are the
same and that ``k1`` and ``k2`` are the same. So the two parameter dependent
data type ``Proxy`` can now be represented:

```haskell
data Proxy k (a :: k) = P
```

Has the following well-typed inhabitants:

```haskell
x :: Proxy * Int
x = P

y :: Proxy Bool True
y = P
```

All types and all constructors can be promoted with -XDataKinds (which is
implied by -XTypeInType), including GADT constructors!

```haskell
data TyRep1 :: * -> * where
  TyInt1 :: TyRep1 Int
  TyBool1 :: TyRep1 Bool

data TyRep :: k -> * where
  TyInt :: TyRep Int
  TyBool :: TyRep Bool
  TyMaybe :: TyRep Maybe
  TyApp :: TyRep a -> TyRep b -> TyRep (a b)

zero1 :: forall a. TyRep1 a -> a
zero1 TyInt1 = 0
zero1 TyBool1 = False
```

And type classes and families can be indexed by kinds now.

```haskell
data Proxy a

class C t where 
  f :: Proxy (a :: t)

instance C Type where 
  f = undefined
```

<hr/>

#### Stack

* *[Stack](https://github.com/commercialhaskell/stack)*

Stack is an supplement to the Cabal build system, released by FPComplete, which
greatly simplifies package installation and dependency resolution. I was
particularly skeptical about yet another solution to package, but it has since
become a regular part of my Haskell workflow and the cost to migrating and
existing codebase to it is often a single command (``stack init``).

The one additional file that is required is a ``stack.yaml`` file in addition to
cabal file.

```yaml
resolver: lts-3.16
packages:
- '.'
extra-deps: []
flags: {}
extra-package-dbs: []
```

For new projects the setup process is very streamlined.

```bash
$ stack new scotty-hello-world
```

The appropriate project directory is automatically.

```bash
├── app
│   └── Main.hs
├── LICENSE
├── scotty-hello-world.cabal
├── Setup.hs
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs
```

The equivalent cabal commands will launch the shells, compile the binary, and
install; all while taking care of any dependencies involved to do these tasks.

```bash
$ stack repl
Configuring GHCi with the following packages: scotty-hello-world
GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( scotty-hello-world/src/Lib.hs, interpreted )
[2 of 2] Compiling Main             ( scotty-hello-world/app/Main.hs, interpreted )
Ok, modules loaded: Lib, Main.
*Main>
```

```bash
$ stack build
Installing executable(s) in /.stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin
Registering scotty-hello-world-0.1.0.0...

$ stack install
Copied executables to /home/sdiehl/.local/bin:
- scotty-hello-world-exe
```

Surprising to me lately was to find how simple integrating Stack and containers
solutions has become.

```yaml
# stack.yaml
resolver: lts-3.16
packages:
- '.'
extra-deps: []
flags: {}
extra-package-dbs: []
image:
  container:
    name: example
    base: hello:base
    entrypoints:
      - hello
```

And the Docker image can be created quite simply by the following commands.

```bash
$ docker build -t hello .
$ stack image container
$ sudo docker run -t -i example hello 
```

```haskell
> eitherDecode "[]" :: Either String Int
Left "Error in $: expected Int, encountered Array"
```

My skeleton [project template is available here](https://github.com/sdiehl/skeleton).

<hr/>

#### Aeson

*[Aeson](https://hackage.haskell.org/package/aeson)*

The bread and butter (and debatably alternative base) library Aeson got much
better error reporting as of ``aeson==0.10``. Before we would often get cryptic
messages concerning attoparsec internals ``Failed reading: satisfy``.

```haskell
> eitherDecode "[]" :: Either String Float
Left "Error in $: expected Float, encountered Array"
```

```haskell
> set -XDeriveGeneric

> data Point = Point { x :: Float, y :: Float}  deriving (Generic, Show)

> instance FromJSON Point

> eitherDecode "{\"x\": 1}" :: Either String Point
Left "Error in $: The key \"y\" was not found"
```

<hr/>

#### Haskell for Mac

*[Haskell for Mac](https://itunes.apple.com/au/app/haskell/id841285201)*

Manuel M T Chakravarty et all, have released a new Playground environment aimed
at lowering the barrier to entry in Haskell.

<!--
<img src="https://images.typed.com/9df5078d-fbf8-4c17-8ef2-d19d47365f46/Haskell_Window_Anatomy.jpg" width=600></img>
-->

The platform is commercial software available on the Apple Store but is well
worth the low cost for such a nicely integrated and polished introduction to the
Haskell language.

#### Code Generation

*[Type-safe Runtime Code Generation](https://www.cse.unsw.edu.au/~chak/papers/acc-llvm.pdf)*

A topic of great interest to me is the Haskell interface to the LLVM code
generation system, which is quickly the de-facto standard for backend
intermediate languages. The
[llvm-general](https://hackage.haskell.org/package/llvm-general) provides a
phenomenal interface to generating machine code from Haskell but the underlying
C++ library itself is rather brittle under the ingestion of invalid LLVM IR.
Trevor McDonald et. all provided a new approach by which many semantic
guarantees of the IR can be represented in the Haskell type system.

*[Call-By-Push-Value](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.129.8945&rep=rep1&type=pdf)*

On the topic of evaluation methods there is interesting research opportunity
into exploring an alternative evaluation methods for the lambda calculus under
the so-called "Call-By-Push-Value" method. Surprisingly this technique has been
around for 11 years but lacks an equivalent of SPJ's seminal "stock hardware
implementation" paper describing an end to end translation from CBPV to x86.
This would be an interesting student project.

<hr/>

#### Web Programming

* *[Servant](https://github.com/haskell-servant/servant)*

In typical Haskell tradition, Servant is a wildly different approach to
designing type-safe Haskell web application interfaces. Using many of the
promoted datatype techniques (DataKinds, PolyKinds, TypeOperators) it is able to
statically guarantee many things that would be be pushed to runtime in other
languages and make a large amount of invalid API states inexpressible. On top of
that The capacity to automatically [generate HTTP
documentation](https://haskell-servant.github.io/tutorial/docs.html) that
automatically evolves with the code is a killer feature.

I find Servant terribly interesting and will track it's development over the
next year, I think it's certainly the shape of things to come in the Haskell web
space. My only (non-technical) concern is that my applications are increasingly
becoming impenetrable to those who are not deeply immersed in the lore of GHC
extensions. It would be rather difficult to spin someone up on this who has not
had at least several months of training about how to write Haskell and interpret
the rather convoluted type-level programming error messages that often emerge.

```haskell
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
```

The "Hello Web App" programs of unsafe Javascript definitely has a psychological
appeal to current generation of developers over a wall of impenetrable type
system extensions.

```javascript
var http = require('http');

var server = http.createServer(function (request, response) {
  response.writeHead(200, {"Content-Type": "text/plain"});
  response.end("Hello World\n");
});

server.listen(8000);
```

This is not a critique of Servant, merely a retrospective about how far the gap
between the Javascript and Haskell worlds is becoming and how much activation
energy is required to cross the schism.

<hr/>

#### Numerical Programming

* *[Dimensional](https://hackage.haskell.org/package/dimensional-1.0.1.0/docs/Numeric-Units-Dimensional.html)*

Dimensional finally takes many of the modern language extensions that have been
present since 7.6 (type families, closed type families and promotion) and
applies them to the problem of building semantically sound dimensional
calculations.  The example [astro
haskell](https://htmlpreview.github.io/?https://raw.githubusercontent.com/DougBurke/astro-haskell/master/html/angular%20diameter%20distance.html)
notebooks by Doug Burke are a great example of these used to do calculations.

* *Mythical Haskell Array Library*

On this topic, I'm a bit more pessimistic. I don't forsee a complete story to
the NumPy/Pandas story in Haskell anytime soon. I strongly suspect that it's
simply not possible to create a library in Haskell that would draw the same kind
of man-hours from scientific practitioners to see the library to fruition.
Progress in the numerical Python space was hard-won and usually built on the
graves of academic careers of people who become community organizers around
projects like NumPy and SciPy. 

There are [several novel approaches]([https://acowley.github.io/Frames/) which
moved the state of things forward, but the use of TemplateHaskell and PolyKinded
datatype programming some these solutions a tough sell at say, at the front desk
of an investment bank. There are very few solutions for which I suggest dynamic
typing is a good solution, but I'm coming more and more to believe that in the
domain of ad-hoc data analysis, dynamic typing hits the local maxima of
usability given our current understanding of languages.

<hr/>

#### Javascript

> JavaScript, the language, has some issues that make working with it
> inconvenient and make developing software harder.

The entire Javascript "phenomenon" is well described in [Alan Kay's
talk](https://www.youtube.com/watch?v=YyIQKBzIuBY) on the results of
["incremental problem
solving"](http://cube-drone.com/comics/c/relentless-persistence) and the
programming pop culture. Nevertheless JS exists, has universal browser support
and is at least retargetable from more sensible languages.

*[Elm](http://elm-lang.org/)*

Elm is another purely functional Javascript tranpiler, it uses Haskell like
syntax but is semantically is closer to early ML dialects. As of yet, there is
rich polymorphism or higher-kinded types, so a whole family of the usual
functional constructions (monoids, functors, applicatives, monads) are
inexpressible. In 2016 when the languages evolves a modern type system I will
give it another look. At the moment it is hard to say much about Elm.

*[Purescript](http://www.purescript.org/)*

Purescript has evolved into a very sensible transpiling solution in 2015, with a
robust compiler and well structured set of base packages. The type system is
superset of Haskell 2010 and expressive enough that the full set of common
functional programming idioms are expressible.

```haskell
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class (Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a

class (Applicative m, Bind m) <= Monad m
```

Beyond the Haskell scope, Purescript adds first class records and row
polymorphism.

```haskell
point2D = { x: 3, y: 4 }

point3D = { x: 3, y: 4, z: 5 }

distance :: { x :: Number, y :: Number } -> Number
distance point = sqrt(point.x^2 + point.y^2)
```

Inspired by Daan Leijen's "Programming with Row-polymorphic Effect Types",
Purescript uses a fine grained effect tracking system based on extensible rows.
For instance the following two functions ``print`` and ``random`` have different
effects during evaluation that are manifest explicitly in their type signature
as a record inside of Eff.

```haskell
random :: forall eff1. Eff (random :: RANDOM | eff1) Number
print :: forall eff2. (Show a) => a -> Eff (console :: CONSOLE | eff2) Unit
```

When composed inside of the Eff monad multiple types of native effect can be
interleaved into the same computation.


```haskell
main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  n <- random
  print n
```

The code generation is fairly natural mapping of Haskell lambda to Javascript
functions, delegation to underlying Javascript builtins for primitive
operations, and the usual dictionary passing translation for typeclasses.

```javascript
var $dollar = function (f) {
    return function (x) {
        return f(x);
    };
};
```

```javascript
var show = function (dict) {
    return dict.show;
};

var showNumber = new Show($foreign.showNumberImpl);

var showBoolean = new Show(function (_35) {
    if (_35) {
        return "true";
    };
    if (!_35) {
        return "false";
    };
    throw new Error("...");
});
```

There are a variety of bindings to external Javascript DOM libraries including
[React](https://github.com/purescript-contrib/purescript-react).  Until the
browser becomes targetable in a way that [doesn't
involve](https://hacks.mozilla.org/2015/12/compiling-to-webassembly-its-happening/)
transpiling source code, Purescript is as state of the art as it gets.

<hr/>

#### Editor Integration

*[haskel-vim-now](https://github.com/begriffs/haskell-vim-now)*

Vim integration with Haskell has never been easier and a lot of the problems
around ghc-mod and syntax highlighting have been packaged up and managed by
stack now by the ``haskell-vim-now`` package. This is markedly improved from
when I last wrote about vim and Haskell in 2013.

*[mote](https://github.com/imeckler/mote)*

With the advent of type holes (at both the value and type level now) there
exists are an interesting set of opportunities for providing Agda-like editor
integration that is syntax and type aware. Specifically case expansion is a much
loved feature that is still missing from our current ghc-mod tooling. Mote is an
exploration of such ideas.

*[haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)*

The Haskell IDE Engine is a community effort to provide a universal IDE
interface that abstracts over the common functionality needed to build future
tooling. *The Haskell tooling dream is near!*

<hr/>

#### SQL

In the past SQL has been a particularly sore spot in the Haskell ecosystem,
although typically the low-level bindings are present the problem of building a
easy to use, type-safe, database-agnostic, language-integrated query system that
maps to Haskell data structures has been ongoing. Opaleye does not fall back on
TemplateHaskell, and although not database agnostic, is closest to what I
imagine the ideal solution would be.

* *[esqueleto](https://hackage.haskell.org/package/esqueleto)*
* *[opaleye](https://hackage.haskell.org/package/opaleye)*
* *[relational-record](https://hackage.haskell.org/package/relational-record)*

I have yet to encounter a set of abstractions that brings the ideas of Philip
Wadler's [A Practical Theory of Language-Integrated
Query](http://homepages.inf.ed.ac.uk/jcheney/publications/cheney13icfp.pdf) to
fruition in Haskell. There is much work in this space on both the engineering
and research side; yet I'm quite confident with some changes that are coming in
GHC 8.0 that this will converge on a nice tight solution.

#### Programming Languages

Several new experimental languages have emerged that are looking into some
radically different ideas about the way we structure programs. Not surprisingly
many of them are bootstrapped on top of Haskell.  It's increasingly becoming
simpler to build language prototypes as many of the component pieces are being
abstracted out into libraries.

* *[Morte](https://github.com/Gabriel439/Haskell-Morte-Library)*
* *[Unison](https://github.com/unisonweb/platform)*
* *[Shem](https://github.com/xixixao/Shem)*
* *[Koka](http://www.rise4fun.com/koka/tutorial)*
* *[Egison](https://hackage.haskell.org/package/egison)*
* *[Shentong](https://github.com/mthom/shentong)*

For those who are
[asking](https://www.reddit.com/r/haskell/comments/3qgsx1/whats_the_equivalent_of_modern_compiler/cwf3fhg),
I am continuing to write the [proto functional compiler
tutorial](http://dev.stephendiehl.com/fun/) and will continue to do so in 2016.
Technical writing is just a very thankless and time-consuming process.

<hr/>

#### Dependent Types

*[Idris](https://github.com/idris-lang/Idris-dev)*

Although technically not a recent development, Idris has seen a lot of work in
2015. Idris is very promising foray into the next generation of dependently
typed languages that actually aims for a full end-to-end compiler. Here is
the clearest preview of what functional programming will look like in 2025.

That said, for the work that I do on a daily basis I'm a bit reserved about the
offerings of dependently typed languages; having spent a fair bit of time
investigating possible use cases for dependently typed numerical programming.
This year may be the one that changes my mind on this, but for the moment
dependent types don't quite yet offer the same power to weight ratio as my usual
System-F and ML derivative languages do. The primary criterion I would use for
considering the next generation of dependently typed languages is when the first
self-hosting optimizing compiler emerges. Until then I remain skeptical but
optimistic about the advances in dependent types.

<hr/>

#### Education & Community

It was good year for the community. There were at least 3 more books written,
thousands of StackOveflow answers added, and the Haskell subreddit grew by 4,000
users.

There is still some active debate interplay of types, laws, and documentation.
Although the overall state of library documentation has gotten uniformly better
over the last year. It is certainly a lot better than many years ago when
understanding any bit of Haskell code was more a test of one's academic library
skills than programming.

The [Haskell Exchange 2015 Park
Bench](https://skillsmatter.com/skillscasts/6739-park-bench-discussion)
discussion is a very good representation of many of the concerns around the
growing community and the debates around "Haskell as stable industrial language"
and "Haskell as vehicle for compiler research" that often occur.
