---
title: Reflecting on Haskell in 2016
date: December 23, 2016
---

### Reflecting on Haskell in 2016

Well, 2016 ... that just happened. About the only thing I can put in perspective
at closing of this year is progress and innovation in Haskell ecosystem. There
was a lot inspiring work and progress that pushed the state of the art forward.

This was a monumental year of Haskell in production. There were dozens of talks
given about success stories with an unprecedented amount of commercially funded
work from small startups to international banks. Several very honest accounts of
the good and the bad were published, which gave us a rare glimpse into what it
takes to plant Haskell in a corporate environment and foster it's growth. 

1. [A Founder's Perspective on 4 Years With Haskell](http://baatz.io/posts/haskell-in-a-startup/) by Carl Baatz
1. [The Story of Haskell at IMVU](https://chadaustin.me/2016/06/the-story-of-haskell-at-imvu/) by Chad Austin
1. [Four Months with Haskell](https://lexi-lambda.github.io/blog/2016/06/12/four-months-with-haskell/) by Alexis King
1. [The Joy and Agony of Haskell in Production](http://www.stephendiehl.com/posts/production.html) by Stephen Diehl
1. [Production Haskell](https://www.youtube.com/watch?v=AZQLkkDXy68&t=1s) by Reid Draper

#### Writing

There was a lot of excellent Haskell writing this year. One can't possible
enumerate all of them, but several stood out as concise and mind-bending
examples of practical Haskell:

1. [Raytracing Black Holes with Haskell](https://flannelhead.github.io/projects/blackstar.html) by Sakari Kapanen
1. [Exploratory Haskell](http://www.parsonsmatt.org/2015/12/09/exploratory_haskell.html) by Parsons Matt
1. [State of the Haskell Ecosystem](http://www.haskellforall.com/2016/02/state-of-haskell-ecosystem-february.html?m=1) by Gabriel Gonzalez
1. [Introducing the Hamilton Library](https://blog.jle.im/entry/introducing-the-hamilton-library.html) by Justin Le
1. [Practical Dependent Types in Haskell: Type Safe Neural Networks](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html) by Justin Le
1. [Mutual Recursion in Final Encoding](https://aherrmann.github.io/programming/2016/05/28/mutual-recursion-in-final-encoding/) by Andreas Herrmann
1. [An Algebra of Graphs](https://blogs.ncl.ac.uk/andreymokhov/an-algebra-of-graphs/) by Andrey Mokhov
1. [GHC optimization and fusion](https://www.stackbuilders.com/tutorials/haskell/ghc-optimization-and-fusion/) by Mark Karpov
1. [Hindley-Damas-Milner tutorial](https://github.com/quchen/articles/tree/master/hindley-milner#readme) by David Luposchainsky
1. [Write You a Scheme 2.0](https://wespiser.com/writings/wyas/home.html) by Adam Wespiser
1. [Proving Stuff in Haskell](http://madsbuch.com/blog/proving-stuff-in-haskell/) by Mads Buch
1. [Making Movie Monad](https://lettier.github.io/posts/2016-08-15-making-movie-monad.html) by David Lettier   
1. [Selling Haskell in the Pub](https://neilmitchell.blogspot.com/2016/02/selling-haskell-in-pub.html) by Neil Mitchell
1. [Little Languages](http://dlaing.org/little-languages/) by David Laing

The second edition of Graham Hutton's book [Programming in
Haskell](https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229/ref=dp_ob_title_bk)
was released, which updated the presentation to include modern Haskell including
recent changes concerning applicative, monadic, foldable, and traversable types.

In addition there were several great lecture series by Bartosz Milewski on
relevant Haskell topics that are generally underserved in writing.

1. [Parallel and Concurrent Programming in Haskell](https://www.youtube.com/watch?v=N6sOMGYsvFA)
1. [Category Theory](https://www.youtube.com/watch?v=I8LbkfSSR58&t=221s&index=1&list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_)

#### Uncategorized Projects

There was a lot of open source work done this year, a few that stood out as
particularly novel ideas or interesting ways of looking at problems through a
Haskell lens:

1. [grenade](https://github.com/HuwCampbell/grenade) A dependently typed, practical, and fast neural network library.
1. [implicitCAD](https://github.com/colah/ImplicitCAD) A math-inspired CAD engine in Haskell. 
1. [hylogen](https://github.com/sleexyz/hylogen) GLSL embedded in Haskell.
1. [sparkle](https://github.com/tweag/sparkle) Haskell on Apache Spark.
1. [bench](https://github.com/Gabriel439/bench) Command-line benchmark tool.
1. [roper](https://github.com/oblivia-simplex/roper) A return-oriented programming exploit toolkit using genetic programming.
1. [tensor-ops](https://github.com/mstksg/tensor-ops) Type-safe tensor manipulation operations in Haskell
1. [formal-morality](https://github.com/alexbecker/formal-morality/) A Pseudo-Rawlsian domain language in Haskell. 
1. [protobuf-simple](https://github.com/sru-systems/protobuf-simple) A simpler Protocol Buffers library for Haskell.
1. [rts-loader](https://github.com/DanielG/rts-loader) A Haskell dynamic RTS loader supporting multiple GHC APIs.
1. [hge2d](https://github.com/I3ck/HGE2D) 2D game engine written in Haskell.

#### Haskell Sucks

On a reflective note, there were quite a few [dogpile
threads](https://www.reddit.com/r/haskell/comments/4f47ou/why_does_haskell_in_your_opinion_suck/)
in which we collectively ripped on our beloved language for it's many flaws. The
responses ranged from the obvious to the absurd, but the most voted flaws (in
order) were:

1. Strings
2. No documentation
3. Records
4. Prelude with partial functions
5. Ambigious space/time complexity
6. No stack traces
7. Language extension overload
8. Long compilation times
9. No IDE

There's some truth to most of these. There's also a answer or workaround to all
of them except documentation. 

#### Editor & IDEs 

The traditional editors saw incremental improvements and much was written about
provisioning development environments for Haskell.

1. [IntelliJ plugin for Haskell](https://rikvdkleij.github.io/intellij-haskell/)
1. [Setting Up A Haskell Development Environment (Mac OS)](http://www.prigrammer.com/?p=332)
1. [Vim and Haskell in 2016 on OS X](http://blog.jez.io/2016/08/03/vim-and-haskell-in-2016-on-os-x/)

FP Complete released the wonderful
[Intero](https://github.com/commercialhaskell/intero) a background process for
interacting with Emacs to do type assisted programming and completion. Plugins
were also created to integrate Intero with the [Neovim
editor](https://github.com/parsonsmatt/intero-neovim).

[Haskell for Mac](http://haskellformac.com/) continued to develop this year
adding new tutorial content and an interactive playground for live programming
using HTML and SVG. It remains probably the simplest way to teach students using
the Macintosh operating system.

I gave a talk on [editor tooling](http://dev.stephendiehl.com/editor_talk.html)
in Boston and updated the [vim
tutorial](http://www.stephendiehl.com/posts/vim_2016.html) as well.

[HyperHaskell](https://github.com/HeinrichApfelmus/hyper-haskell) was released
which provides an IPython/Mathematica style workbook for interactive Haskell
development. It is cross-platform and runs on Linux, Mac and Windows. Hyper
extends Haskell's Show mechanism to support with a ``Display`` typeclass which
can be overloaded to display graphics, mathematics, diagrams or structured HTML
when rendering Haskell values to the workbook. 

<img src="https://github.com/HeinrichApfelmus/hyper-haskell/raw/master/docs/screenshots/worksheet-diagrams.png" width=400/>

On a future note, industry programmers using tools like Slack, Atom, and Visual
Studio Code seem quite happy using web applications disguised in Chromium
disguised as a native application using frameworks like
[Electron](http://electron.atom.io).  Obviously this isn't a perfectly optimal
solution, but there is definitely room for an ambitious team to take the
prebuilt haskell-ide-engine, FP Complete
[haskell-ide](https://github.com/fpco/haskell-ide) or intero backends and shape
it into an Haskell specialized IDE environment developers who like such a fully
featured environment. Perhaps the FP Complete IDE was a good idea, just a bit
early.

#### GHC

The Glorious Glasgow Haskell Compiler had it's 8.0 release, and landed to to
much rejoicing. It was a big release and landed quite a few new features. It
also staged quite a bit of partial work that will be manifest in the 8.2 which
is tentatively scheduled for release candidate in mid-February 2017 and release
in mid-April 2017. The GHC core development team did some truly progressive work
this year.

**Type In Type**

Haskell's pseudo-dependent type solution ``TypeInType`` landed. The initial
solution was a bit brittle and is not actively used too much in the wild.
Simply put the ``TypeInType`` extension removed the distinction between types of
kind ``*`` and types of other kinds.

```haskell
λ> :set -XTypeInType 
λ> import Data.Kind 
λ> :info Type
type Type = * 	-- Defined in ‘GHC.Types’
λ> :kind Type
Type :: *
```

Richard Eisenberg finished up the work for his
[thesis](https://github.com/goldfirere/thesis/blob/master/built/thesis.pdf) and
released a status report on the
[efforts](https://typesandkinds.wordpress.com/2016/07/24/dependent-types-in-haskell-progress-report/).

**Type Family Dependencies**

Type families historically have not been injective, i.e. they are not guaranteed
to maps distinct elements of its arguments to the same element of its result.
The syntax is similar to the multiparmater typeclass functional dependencies in
that the resulting type is uniquely determined by a set of the type families
parameters. GHC 8.0 added support for this with the ``TypeFamilyDependencies``
extension.

```haskell
type family F a b c = (result :: k) | result -> a b c
type instance F Int  Char Bool = Bool
type instance F Char Bool Int  = Int
type instance F Bool Int  Char = Char
```

**Kind Equalities**

Up until GHC 8.0, we've not been able to express explicit kind equality proofs.
As a result, type-level computation does not have access to kind level functions
or promoted GADTs, the type-level analogues to expression-level features that
have been so useful.

For instance in Agda if we wanted to write down a type-level proof about the
commutativity of addition over the natural numbers we can do this quite simply.
We use the usual propositional equality to express the type that two things are
equal using substitutivity (``subst``): for any proposition (type), we can
replace a term with a propositionally equal one, without changing the meaning of
the proposition. And relatedly congruence (``cong``): if any function f respects
propositional equality, it yields propositionally equal results if applied to
propositionally equal arguments.

```haskell
data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

infixl 6 _+_

_+_ : ℕ → ℕ → ℕ
0 + n = n
suc m + n = suc (m + n)

infix 4 _≡_

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

subst : {A : Set} → (P : A → Set) → ∀{x y} → x ≡ y → P x → P y
subst P refl p = p

cong : {A B : Set} (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

assoc : (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
assoc zero    _ _ = refl
assoc (suc m) n p = cong suc (assoc n p)
```

As of GHC 8.0 we now have enough to do this kind of kind-level reasoning using
propositional equality with ``TypeInType``. 

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind

 -- pattern matching on Refl introduces proof
data (==) :: forall (t :: *). t -> t -> * where
  Refl :: x == x

data Nat = Z | S Nat

infixl 6 +
type family (+) a b where
  Z   + b = b
  S a + b = S (a + b)

type family Sym (eq :: (x :: a) == (y :: a)) :: y == x where
  Sym Refl = Refl

type family Cong (f :: a -> b) (p :: x == y) :: f x == f y where
  Cong f Refl = Refl

type family Assoc
  (a :: Nat) (b :: Nat) (c :: Nat) :: (a + b + c) == ((a + b) + c)
  where 
    Assoc Z     b c = Refl
    Assoc (S a) b c = Cong S (Assoc a b c)
```

**Visible Type Applications**

Visible Type Applications was added with the ``-XTypeApplications`` flag to
allow the addition of explicit type arguments directly to polymorphic
call-sites. For instance:

```haskell
show (read @Int 42")
```

**Custom Type Errors**

As of GHC 8.0 we have the capacity to provide custom type error using type
families. The messages themselves hook into GHC and expressed using the small
datatype found in `GHC.TypeLits`.

```haskell
data ErrorMessage where
  Text :: Symbol -> ErrorMessage
  ShowType :: t -> ErrorMessage

  -- Put two messages next to each other
  (:<>:) :: ErrorMessage -> ErrorMessage -> ErrorMessage

  -- Put two messages on top of each other
  (:$$:) :: ErrorMessage -> ErrorMessage -> ErrorMessage
```

An example would be creating a type-safe embedded DSL that enforces invariants
about the semantics at the type-level. We've been able to do this sort of thing
using GADTs and type-families for a while but the error reporting has been
horrible. With 8.0 we can have type-families that emit useful type errors that
reflect what actually goes wrong and integrate this inside of GHC. This is going
to a *big deal* for embedded DSL design in Haskell where failures were typically
opaque and gnarly.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits

type family Coerce a b where
  Coerce Int Int     = Int
  Coerce Float Float = Float
  Coerce Int Float   = Float
  Coerce Float Int   = TypeError (Text "Cannot cast to smaller type")

data Expr a where
  EInt    :: Int -> Expr Int
  EFloat  :: Float -> Expr Float
  ECoerce :: Expr b -> Expr c -> Expr (Coerce b c)

foo :: Expr Int
foo = ECoerce (EFloat 3) (EInt 4)
```

**OverloadedRecordFields**

This was a source of great pain in previous releases where common identifiers
like ``id`` would have to have superfluous prefixes associated with each record,
this is no longer an issue with the ``OverloadedRecordFields`` extension.

```haskell

data A = A
  { a :: Int
  , b :: Double
  } deriving Show

data B = B
  { a :: String
  , c :: Char
  } deriving Show

mkA :: A
mkA = A {a = 23, b = 42}

upA :: A -> A
upA x = x {a = 12}

sel :: A -> Int
sel = a

main :: IO ()
main = print $ sel $ upA $ mkA
```

GHC 8.0 also introduced the ``OverloadedLabels`` extension which allows a
limited form of polymorphism over record selectors and updators that share the
same name.

```haskell
class IsLabel (x :: Symbol) a where
  fromLabel :: Proxy# x -> a
```


```haskell
instance IsLabel "true" Bool where
  fromLabel _ = True

instance IsLabel "false" Bool where
  fromLabel _ = False

instance IsLabel "true" Int where
  fromLabel _ = 1

a :: Bool
a = #false

b :: IsLabel "true" t => t
b = #true
```

```haskell
main = do 
  print a
  print (b :: Bool)
  print (b :: Int)
```

**MonadFail**

The MonadFail Proposal finally removed the ugly ``fail`` function that was
historical cruft from the 90s. A new class
[``MonadFail``](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad-Fail.html)
was implemented along with a set of warning flags pattern matching or guard
would introduce a ``fail`` occurrence.

```haskell
class Monad m => MonadFail m where
    fail :: String -> m a
```

**DeriveLift**

8.0 added yet another automatic deriving mechanism. This time we can now
automatically derive the boilerplate for TH Lift instances allowing us to embed
free variables containing ``Lift`` instances inside of the Oxford brackets.

```haskell
{-# LANGUAGE DeriveLift #-}

module Lift where

import Language.Haskell.TH.Syntax

data Expr
  = Zero
  | Succ Expr
  deriving (Show, Lift)
```

And in a separate module:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Exp where

import Lift
import Language.Haskell.TH.Syntax

zero :: Q Exp
zero  = [|Zero|]

suc :: Expr -> Q Exp
suc x = [|Succ x|]

x :: Expr
x = $( [|Succ Zero|] )
```

**Strict Haskell**

Haskell is normally uses a call-by-need semantics such that arguments passed to
functions are only evaluated if their value is used. Bang patterns (written as
``!x``) are explicit annotations that can force a specific to be evaluated
before entering the function instead of as needed. GHC 8.0 introduced the
ability to enable this for all functions in a specific module.

For example given the following set of definitions:

```haskell
data T = C a
f x = y x
let x = y in rhs
case x of y -> rhs
case x of (NewType y) -> rhs
```

Enable ``-XStrict`` will automatically performs the equivalent of adding
strictness annotations to the every argument source. Effectively transforming
the above into the following:

```haskell
data T = C !a
f !x = y x
let !x = y in rhs
case x of !y -> rhs
case x of !(NewType y) -> rhs
```

Enabling will not uniformly increase the performance of code as some care is
required when enabling it. It simply makes a slightly alters the behavior to one
extreme of the space-time compromise spectrum, optimal performance still
requires a clever mix of both laziness and strictness. 

**Stack Traces**

Simon Marlow cracked the perpetual [stack trace
problem](https://simonmar.github.io/posts/2016-02-12-Stack-traces-in-GHCi.html)
allowing a variety of approaches to getting detailed stack trace
informations when we have to panic or unwind the stack.

For explicitly defined functions we can annotate call sites with a
``HasCallStack`` constraint which can be used to to obtain a partial call-stack
at any point in the program. HasCallStack is a type constraint synonym for
threading an implicit parameter ``?callStack``.

```haskell
type HasCallStack = ?callStack :: CallStack
```

```haskell
import GHC.Stack

f :: HasCallStack => Int -> Int
f x = error ("Error: " ++ show x)

g :: HasCallStack => Int -> Int
g = withFrozenCallStack f

main :: IO ()
main = print $ g 23
```

For automatic stack traces across the entire program in GHCi we can compile the
with a specific set of flags and profiling enabled. The precise stack trace from
the closure where assertion was called will then be printed if the term is
evaluated. Currently this introduces a 2-3x runtime overhead when compiled.

```bash
$ ghci -fexternal-interpreter -prof 
```

**Unboxed Sum Types**

GHC 8.2 will have ``UnboxedSums``, that enables unboxed representation for
non-recursive sum types. The extension will be enabled as an opt-in annotation
with new to explicitly unpack selected datatypes for high-performance
structures.

Unboxed types are those which instead of being represented by a pointer to heap
value are passed directly in a CPU register. The usual numerics types in Haskell
can be considered to be a regular algebraic datatype with special constructor
arguments for their underlying unboxed values.

```haskell
data Int = I# Int#

data Integer
  = S# Int#              -- Small integers
  | J# Int# ByteArray#   -- Large GMP integers

data Float = F# Float#
```

The syntax introduced allows sums to be explicitly unboxed by delimiters ``(#
...  #)``.  Pattern matching syntax follows the same form, and allows explicit
unpacking of the unboxed sum in case statements.

```haskell
type Sum1 = 
  (# 
    (# Int#, Int #)
    | (# Int#, Int# #)
    | (# Int, Int# #) 
  #)

showSum1 :: Sum1 -> String
showSum1 (# p1 | | #) = showP1 p1
showSum1 (# | p2 | #) = showP2 p2
showSum1 (# | | p3 #) = showP3 p3
```

The usual ( ``Maybe a`` ) type was traditionally represented by a tagged closure,
with a two pointer lookups to the parameter ``a``. With unboxed sums we simply
store the pointer to the value a alongside the flag for the sum type without the
need for extra indirection. This would effectively allow a 'zero-cost
abstraction' (with full type safety) use of the Maybe monad when fully
optimized.

**Compact Regions**

Support for 'Compact Regions' is planned for GHC 8.2. Compact regions are
manually allocated regions where the data allocated inside it are compacted and
not traversed by the GC. This is amenable for long-lived data structures that
are resident in memory without mutation frequently occurring. William Sewell
gave a great [talk](https://blog.pusher.com/latency-working-set-ghc-gc-pick-two/) at the
London Haskell meetup where the issues with large ``Data.Map`` structures used
for a message bus system caused non-optimal GC performance

The proposed API has been
[expressed](http://ezyang.com/compact/Data-Compact.html) as well as a paper on
the details on extensions to the GC and [runtime
system](http://ezyang.com/papers/ezyang15-cnf.pdf).


**GHC Education**

At the beginning of the year I felt one of the larger looming issues is that GHC
internals are too opaque. However there was a lot of great writing and talks
given this year about practical examples of extending and exploring the GHC
internals. 

Simon Petyon Jones gave a brilliant talk on [Into the
Core](https://www.youtube.com/watch?v=uR_VzYxvbxg), describing the System-FC core
language at the heart of all of Haskell. He also then described the new work on
the [Sequent
calculus](http://research.microsoft.com/en-us/um/people/simonpj/papers/sequent-core/scfp_ext.pdf)
as a compiler intermediate language for future work.

I wrote a three part blog series on the internal GHC types, pipeline, and gave
some practical tutorials on augmenting the compiler with custom logic. Work
permitting I'll try to extend this series next year with posts on CMM and RTS
internals.

1. [Dive into GHC: Part 1](http://www.stephendiehl.com/posts/ghc_01.html)
1. [Dive into GHC: Part 2](http://www.stephendiehl.com/posts/ghc_02.html)
1. [Dive into GHC: Part 3](http://www.stephendiehl.com/posts/ghc_03.html)

Takenobu Tani published a lovely slide deck on [GHC internals
illustrated](https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf).

David Luposchainsky released a [STGj](https://github.com/quchen/stgi), a visual
STG (Spineless Tagless G-Machine) implementation to help understand Haskell's
execution model. A talk on the subject was also given at
[ZuriHac](https://www.youtube.com/watch?v=-MFk7PIKYsg).

Alberto Sade wrote a short tutorial on manipulating the [GHC
Core](http://aesadde.xyz/posts/ghc_core.html) types from inside Haskell
programs.

Christiaan Baaij wrote a [detailed
tutorial](https://christiaanb.github.io/posts/type-checker-plugin/) on extending
the GHC typechecker with a custom plugins which allow more complex logic to be
embedded in the type system.

#### Stack

Since last year Stack has become near ubiquitous. Every company that I've come
in contact with in the last year is using it internally for their builds.  The
tooling makes some compromises on version bounds and compatibility that are
'pragmatic' and actively debated, but overall the tooling has brought more
people into the language and vastly decreased a lot of the friction we once had.
The innovation of Stack can be put quite simply as making all build commands
idempotent and sandboxes as stateless, which was a vast improvement over
stateful sandboxes which would quickly become corrupted or inconsistent and
needed to be rebuilt constantly.

```bash
stack ghci --package protolude
```

Stack has since grown a rich set of integrations with tooling such as Nix,
GHCjs, TravisCI, CircleCI, Docker, Kubernetes as well as broad set of templates
for [quickly starting](https://github.com/commercialhaskell/stack-templates)
Haskell projects.

The alternative ``cabal new-build`` build system is under active development.
The new build system uses Nix-style local builds which makes a distinction
between local packages and external packages. Local packages, which users edit
and recompile and must be built per-project whereas external packages are cached
across projects and retrieved from Hackage. 

1. [New-Build Documentation](https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html)
1. [Announcing Cabal New-Build](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/)

There will likely still be healthy debate about best practices, but despite
whatever approach converges much of the Cabal-hell problems of years past are a
distant memory. We have ubiquitous cached builds, stateless sandboxing and easy
to use-tooling.

#### Probabilistic Programming

Probabilistic domain languages are designed to describe probabilistic models by
allowing variables defined in the language to represent stochastic quantities,
and then perform inference (Markov Chain Monte-Carlo, Gibbs Sampling, etc) on
combinations of these stochastic models according to the structure of the their
combination in the program. Probabilistic programming is an exciting, and
growing, area of research, with fantastic people in both AI/ML and PL working
together and making big strides.  It is especially finding uses in quantitative
finance as of late.

Jared Tobin has written some [excellent
articles](https://jtobin.io/simple-probabilistic-programming) on the topic of
building minimalist domain languages to model probabilistic models. There's also
been several other encodings in libraries like
[hakaru](https://hakaru-dev.github.io/) and [monad-bayes](https://github.com/adscib/monad-bayes).

#### LLVM

LLVM is a compiler toolchain and intermediate language used in most modern
compilers. Since Haskell is heavily used in compiler and language work, the LLVM
bindings are of great interest to a lot of Haskellers. This year saw quite a few
new compilers and derived work.

Roberto Castañeda Lozano released
[Unison](https://github.com/unison-code/unison/tree/master/src/unison) a new
Haskell library which integrates with the LLVM toolchain as an alternative or as
a complementary approach to default register allocation algorithms which are
suboptimal for embedded or low-power systems.

Moritz Angermann gave a lovely talk on using embedded DSLs in Haskell to
generate [LLVM bitcode](https://github.com/angerman/data-bitcode-llvm) and
presented at [ICFP](https://www.youtube.com/watch?v=mrJUjlxoTTY).

I travelled to Paris and Berlin this autumn and gave a short talk on compiling
small native languages using llvm-general Haskell bindings to build a small
compiler called 'Petit'. Andreas Herrmann also gave a brilliant talk on the
subject in Zurich with a more detailed [dive
through](https://www.youtube.com/watch?v=Re3XgFfflzg) through the internals of a
simple LLVM compiler backend.

The Summer of Code sponsored work on the LLVM backend to Accelerate, one of
Haskell's array computing librarys. The work [implemented
all](https://github.com/AccelerateHS/accelerate-llvm/commits?author=ZihengJiang)
of the core operators and the project should be ready widespread soon.

#### Mathematics

The [arithmoi](https://github.com/cartazio/arithmoi) number theory library found
new maintainers and is being actively developed once again.

```haskell
> import Math.NumberTheory.Zeta
> import Math.NumberTheory.Primes.Sieve

> take 10 primes
[2,3,5,7,11,13,17,19,23,29]

> take 3 (zetas 1e-14) :: [Double]
[-0.5,Infinity,1.6449340668482262]
```

A rather popular student project this year was a [implementation of the Wolfram
Language](https://github.com/jyh1/mmaclone), a simple M-Expression language
based on term rewriting. The project demonstrated the obvious strength Haskell
has for modeling mathematical domain languages. There is most definitely a lot
of low-hanging fruit for building the core machinery for classic computer
algebra problems. A basic implementation of the following would go quite far
toward advancing Haskell in computational algebra:

1. *Gröbner basis* calculation for solving systems of polynomials in commutative algebras.
1. *Normal-Risch algorithm* for symbolic integration.
1. *Strong Generating Sets/Coset* algorithms for tensor index canonicalization.
1. *Horner scheme* for evaluation of univariate polynomials over arbitrary domains.


#### Cryptography

Due to commercial investment, cryptography libraries in Haskell have gotten much
more mature this year. We have a complete complement of the industry standard
algorithms for various tasks:

1. [Poly1305](https://hackage.haskell.org/package/cryptonite-0.21/docs/Crypto-MAC-Poly1305.html#g:1) for message authentication codes.
1. [Blake2b](https://hackage.haskell.org/package/cryptonite-0.21/docs/Crypto-Hash-Algorithms.html#g:1) or [Keccak-256](https://hackage.haskell.org/package/cryptonite-0.21/docs/Crypto-Hash-Algorithms.html#t:Keccak_256) for hashing.
1. [Scrypt](https://hackage.haskell.org/package/cryptonite-0.21/docs/Crypto-KDF-Scrypt.html) or [Argon2]( https://hackage.haskell.org/package/argon2-1.2.0/docs/Crypto-Argon2.html) for password key derivation.
1. [Chacha20-256](https://hackage.haskell.org/package/cryptonite-0.21/docs/Crypto-Cipher-ChaCha.html#g:1) for symmetric key encryption.
1. [Curve25519](https://hackage.haskell.org/package/cryptonite-0.21/docs/Crypto-PubKey-Curve25519.html) for elliptic curve public key cryptography.

Also a friendly reminder that the NSA has likely developed the capability to
decrypt a large number of HTTPS, SSH, and VPN connections using an attack on
common primes used in the parameters of the Diffie-Hellman key exchange
algorithm with keys less than 2048 bits. Some estimates also put decryption with
the D-Wave computers within 10 years which poses a threat to current
cryptographic approaches based on the discrete-logarithm problem in finite fields
and elliptic curves.  Be safe as we go into uncertain territory in 2017.

At ICFP there was a [fascinating
presentation](https://www.youtube.com/watch?v=RA0DsMrwcxo) on interesting work
on implementing [somewhat homomorphic encryption
(SHE)](https://eprint.iacr.org/2015/1134.pdf#page=24) and lattice cryptography
in Haskell.

#### Committees

The Haskell 2020 committee formed with the goal of forming a new language report
that GHC 8.8 would implement. There is a mailing list for [haskell
prime](https://mail.haskell.org/pipermail/haskell-prime/) as well as several RFC
projects on Github to track the progress. The GHC project also started tracking
proposal discussion on Github using pull requests in a more public
[forum](https://github.com/ghc-proposals/ghc-proposals/pulls).

The [DataHaskell](https://github.com/DataHaskell) github organization was formed
with the stated goal of improving the environment for data science in Haskell.
It has since become an incubator for several ambitious projects to advance the
state of numerical computing libraries and documentation. 

#### Serialization

Well-Typed and FPComplete both released two binary serialization libraries.

1. [binary-serialise-cbor](https://github.com/well-typed/binary-serialise-cbor)
1. [store](https://hackage.haskell.org/package/store)

Both are high performance serialization libraries with different encodings,
endianness choices and performance characteristics, especially around fixed-size
vectors and traversals times of input types.

#### HalVM

Development on HalVM out of Galois picked up with some public statements on
project. HalVM is a framework for writing Haskell programs that run lightweight
virtual machines run directly on the Xen hypervisor. This potentially opens up
the opportunity for deploying "containers" that are single purpose programs with
minimal exposure to the usual baggage Linux operating system carry and their
potential exploits.

1. [HalVM Status 1](http://uhsure.com/halvm-status1.html)
1. [HalVM: The Vision](http://uhsure.com/halvm3.html)

Tooling was also published to deploy these Unikernels to standard Amazon EC2
infrastructure. See: [ec2-unikernel](https://github.com/GaloisInc/ec2-unikernel)

#### Languages

Los Alamos National Laboratory released an interesting
[auto-parallelizing](https://tfp2016.org/papers/TFP_2016_paper_14.pdf) Haskell
subset called [APPFL](https://github.com/losalamos/APPFL).

[microML](https://github.com/kellino/microML) is a simple functional programming
language designed for teaching at University College of London for the BBC
micro:bit microcomputer.

Fugue developed an internal language called [Fugue](https://fugue.co/) which is
used for automating cloud deployments. The technology was demoed at [ICFP
talk](https://www.youtube.com/watch?v=rIphd57Sm1U) this year.

Summer of Code sponsored work on a ["blocky"
interface](https://code.world/blocks) for composing Haskell programmers, used
for teaching basic functional programming.

Swift Navigation released [Plover](https://github.com/swift-nav/plover), an
embedded Haskell DSL for compiling linear algebra routines into C for running on
embedded systems.

Daan Leijen continued development on [Koka](https://github.com/koka-lang/koka)
an experiment in effect typing using row polymorphism.

[The Corrode](https://github.com/jameysharp/corrode) project was introduced
which provided a semantics-preserving automatic translation tool from C to Rust,
for migrating legacy code. Using Haskell to remove legacy C code from this world
can only be a good thing.

Paul Chiusano continued development on
[Unison](http://unisonweb.org/2016-10-12/search.html#post-start) a 'next
generation programming platform' implemented in Haskell and designed for
building large-scale distributed systems.

Gabriel Gonzalez released
[Dhall](https://hackage.haskell.org/package/dhall-1.0.1/docs/Dhall-Tutorial.html),
a total non-Turing complete programming language specialized for configuration
files.  Including a standard library hosted on permanent web
[IPFS](https://ipfs.io/ipfs/QmcTbCdS21pCxXysTzEiucDuwwLWbLUWNSKwkJVfwpy2zK/Prelude)
node.

#### Prelude

The single largest reported pain point for the Haskell language is simply from
certain "naughty" historical things in the Prelude which no longer reflect
modern thinking. The default Prelude makes it much too accidentally shoot oneself
in the foot by using suboptimal String types, partial functions, impure
exception throwing, and a variety of other paper cuts.

Earlier this year I released my perspective on the issue: Protolude. Protolude
is likely the least ambitious alt-prelude ever. It doesn't do anyting new, just
fixes Base exports with the legacy bits masked and much of the mental overload
of String conversions smoothed over. Notably it maintains ABI compatibility with
existing Haskell code. Judging by the number of uses of Github it seems
well-received and is being used in the core of major projects like PostgREST and
Purescript.

There are plenty of other approaches to [prelude
design](https://hackage.haskell.org/packages/#cat:Prelude) and I believe the
explosion is a healthy reaction to a standard library which many users consider
not suitable for industrial use. No one prelude will be suitable for everyone
for all use cases. The two concerning factors to keep in mind when exploring
this space are:

1. Keep the transitive dependency tree small.
2. Maintain compatibility with base whenever possible.

For a new projects in 2017, consider just starting with ``NoImplicitPrelude``
in  your cabal file and importing a sensible set of defaults from your library
of choice.

```haskell
default-extensions:
  NoImplicitPrelude
```

Or use a stack template like:

```bash
$ stack new protolude
```

#### Foundation

Alternate preludes are only routing around the problem because of the
constraints of being unable to fix problems upstream. The [Foundation]( library
is https://github.com/haskell-foundation/foundation) bit grander in scope and
aims to fix quite a few of the overarching problems with the ecosystem.

1. UTF8 based strings are default and are stored as packed array of codepoints.
1. FilePath are algebraic datatypes and can be manipulated and inspected without
   string munging.
1. IO actions read into ByteStrings and conversion into the appropriate type is
   left to the user.
1. Partial functions are either removed or wrapped in a ``Partial``  monad.
   Non-empty datatypes are provided by default.
5. An abstract container interface is provided that allows multiple data
   structures (Set, Map, Vector, etc) to use the same functions overloaded by a
   type class with associated data families to track elements or index types.
6. More granular numerical tower that doesn't require partial implementations of
   functions that aren't relevant.

For a quick taste:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Foundation
import Foundation.IO
import Foundation.String
import Foundation.VFS.FilePath

import Foundation.Collection

example :: String
example = "Violence is the last refuge of the incompetent."

bytes :: UArray Word8
bytes = toBytes UTF8 example

file :: IO (UArray Word8)
file = readFile "foundation.hs"

fileString :: IO (String, Maybe ValidationFailure, UArray Word8)
fileString = fromBytes UTF8 <$> file

xs :: NonEmpty [Int]
xs = fromList [1,2,3]

x :: Int
x = head xs
```

I'm not aware anyone currently using this library, but it most certainly
something to watch as it matures in 2017. It might be the "new hope" we've been
looking for which consolidates industrial Haskell best practices. 

#### Generics

Generics in 8.0 extended the generics structure to include a richer set of
queryable data. We now have full access to the lifted status, strictness
annotations, datatype names, selector names, constructors, and newtype status of
all symbols in a Generic instance.

The implementation of ``DeriveAnyClass`` pragma has also allowed classes which
provide fully default signatures to automatically derived without empty instance
declarations. Aeson for instance can now automatically derive JSON serializes
and de serializes for any instance of Generic for free.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Text
import Data.Aeson

data Sample = Sample
  { a :: Text
  , b :: Text
  } deriving (Show,Generic,FromJSON,ToJSON)

-- decode { "a": "foo", "b": "bar" } :: Sample
```

Libraries like ``optparse-generic`` have implemented prototypes of building
entire command line interfaces using generics on top of vanilla Haskell data
structures.

Andres Löh gave an hour long lecture on the new approach of using generic
sums-of-products with generics-sop at [ZuriHac
2016](https://www.youtube.com/watch?v=sQxH349HOik).

I wrote a [short blog](http://www.stephendiehl.com/posts/generics.html) post
amount implementing custom generics functionality and how the internals of
Generic deriving works.

Ryan Scott wrote a detailed blog post detailing all of the [substantial
changes](https://ryanglscott.github.io/2016/05/12/whats-new-with-ghc-generics-in-80/)
in GHC 8.2 and changes to metadata format. 

#### Idris

Idris is a pure functional language with full dependent types, and is itself
written in Haskell. Idris is not likely to be the tool I'll reach for most of my
day-to-day work, but it's maturing quite rapidly and pushing the envelope of
practicality in dependent types more so than any other language. It was
announced that Idris is heading toward a [1.0
milestone](http://www.idris-lang.org/towards-version-1-0/).

There is also initial work on a [Java Virtual Machine
backend](https://github.com/mmhelloworld/idris-jvm) to Idris to supplement the
builtin native code generator and Javascript backends.

There is also work on porting Benjamin Pierce's seminal work 'Software
Foundations' into [Idris](https://github.com/idris-hackers/software-foundations).

Most notably 0.9 introduced elaboration reflection using Idris as its own
metalanguage. To explain, Idris is type checked in two stages: first, a
metaprogram known as an elaborator translates the surface language into a small
core language (called TT), after which the resulting TT program is type checked
. Incomplete programs can be introduced as holes, which in the context of the
rest of the program, have a set of unification constraints to be solved. 

Elaboration allows us to 'script' the automatic completion of these holes with
reusable logic that can access the full typing environment. This used to exist
in a dedicated [tactic system](http://docs.idris-lang.org/en/latest/reference/tactics.html) (ala Coq)
but now these tactics can themselves be expressed using the elaborators
reflection in Idris itself using a small set of monadic combinators.

```haskell
-- Convert a hole to make it suitable for bindings
attack : Elab ()

-- Introduce a lambda binding around the current hole and focus on the body
intro : (n : TTName) -> Elab ()

-- Place a term into a hole, unifying its type.
fill : Raw -> Elab ()

-- Substitute a guess into a hole.
solve : Elab ()
```

For example to elaborate a polymorphic identity function:

```haskell
mkId : Elab ()
mkId = do
  x <- gensym "x"
  attack
  intro x
  fill (Var x); solve
  solve

idNat : Nat -> Nat
idNat = %runElab mkId
```

The latest version now ships with a standard library of proof tactics called
[Pruviloj](https://github.com/idris-lang/Idris-dev/tree/master/libs/pruviloj)
that work with Idris's elaborator. For example we can automate induction proofs
to prove associativity of numerical operations.

```haskell
import Pruviloj

auto : Elab ()
auto = do
  compute
  attack
  try intros
  hs <- map fst <$> getEnv
  for_ hs $ \ih => try (rewriteWith (Var ih))
  hypothesis <|> search
  solve

ind : Elab ()
ind = do
  attack
  n <- gensym "x"
  intro n
  try intros
  ignore $ induction (Var n) `andThen` auto
  solve

assoc : (j, k, l : Nat) -> plus (plus j k) l = plus j (plus k l)
assoc = %runElab ind
```

Will then automatically fill in the term:

```haskell
assoc : (j, k, l : Nat) -> plus (plus j k) l = plus j (plus k l)
assoc Z     k l = Refl
assoc (S j) k l = let rec = assoc j k l in rewrite rec in Refl
```

#### Agda

Agda is the most mature dependently typed functional programming language, and
is itself written in Haskell. It is regularly used for research and for build
complex type-level constructions to extract into Haskell.

Agda actually has quite lovely [documentation
now](https://agda.readthedocs.io/en/latest/) and is far more approachable than
5-6 years ago. There was recently a full course entitled Agda from Nothing
([video](https://www.youtube.com/watch?v=-i-QQ36Nfsk),
[source](https://github.com/scott-fleischman/agda-from-nothing)) by which walks
one Scott Fleischman through the basics of dependently typed programming from
first principles. It's never been easier to learn.

#### Javascript

Javascript unfortunately continues to exist. While I remain skeptical of the
entire transpiring phenomenon and think it will lead to an endless amount of
wasted person-hours and legacy code. However the pragmatist in me also knows
that the browser vendors are not economically incentivized to change the status
quo, so nothing will change quickly and we should adapt ourselves to the least
worst solution.

Last year I said I would revisit Elm if anything changed. Nothing has changed.
It's as uninteresting a language as it was last year. It's 2017, languages
should have a coherent story for polymorphism that doesn't involve manual code
duplication. I remain unconvinced by arguments attempting to reframe 'primitive'
technology as 'simple'.

Purescript on the other hand is increasingly becoming a more mature and robust
language fully grounded in modern ideas. There was a [lovely book
written](https://leanpub.com/purescript/read) this year as well as variety of
language improvements like newtype deriving, generics, type directed search,
source maps, better error reporting, and custom package manager.

The Purescript community developed
[Halogen](https://github.com/slamdata/purescript-halogen/blob/master/GUIDE.md)
which is a  a toolkit for building reactive web apps using signal functions and
a virtual DOM. It exposes a set of combinator for acting on signals which vary
over time and dispatch to DOM event changes. This provides a proposal to the
current trend of React/Redux-style application construction. 

The GHCjs solutuion has gotten more mature and stable, and integrated with the
Stack ecosystem. There were some impressive demos on compiling the entirety of
[Pandoc](http://markup.rocks) into Javascript, which is quite a feat of
engineering. Nevertheless, I remain somewhat skeptical that compiling Haskell to
enormous blobs of auto-generated Javascript that contain the entire Haskell
runtime is a sustainable solution for large commercial codebases. With
WebAssembly on the horizon I might be convinced otherwise in 2017.

#### Eta

[Eta](https://github.com/typelead/eta) is a Java Virtual Machine Backend for GHC
7.10. The stated goal of the project is to compile the entirety of Haskell to
run with full core library and concurrent runtime support. The first release is
estimated in January of 2017.  The codebase is under active development by Rahul
Muttineni and taking contributions.

Most notably the library will first class support for integrating with existing
Java libraries through FFI, much like we do with C today.

```haskell
import Java

data {-# CLASS "java.util.List" #-} List a = List (Object# (List a))
  deriving Class

foreign import java unsafe "@new" newArrayList :: Java c (ArrayList a)
```


#### Formal Verification

Haskell continues to find use in verification and auditing of cryptography.
Although not new developments, both the Galois Cryptol library and Tamarin
Prover (both Haskell-based tools) are being used for industrial strength uses in
high-assurance work. Haskell continues to have best in class integration with
external solvers such as Z3 and CVC4.

1. [Tamarin](https://tamarin-prover.github.io/)
1. [Cryptol](https://github.com/GaloisInc/cryptol)

The National Science Foundation is currently funding work out of University of
Pennsylvania and MIT to build an entire ecosystem of language tools that have
end-to-end correctness proofs called DeepSpec. The
[CoreSpec](http://www.deepspec.org/research/Haskell/) program aims to develop a
formal Coq specification of the GHC Core Language, type system, and semantics.

The Lean theorem prove recently added a complete [Homotopy Type
Theory](https://github.com/leanprover/lean2/blob/master/hott/hott.md) library
with a complete [mapping of the
chapters](https://github.com/leanprover/lean2/blob/master/hott/book.md) from the
textbook.

#### Liquid Haskell

Refinement types allow us to enrich Haskell's type system with predicates that
precisely describe the sets of valid inputs and outputs of functions These
predicates are compiled down into a specific core language which can be
discharged to an SMT solver for which there are fast decision procedures for
testing the validity of the type.

For example we can construct refinements over a ``Int`` variable ``v`` .

```haskell
{-@ type Zero    = {v:Int | v == 0} @-}
{-@ type NonZero = {v:Int | v /= 0} @-}
```

And constrain our function definitions so that the functions carry around a
proof that a non-zero term will not be passed to it.

```haskell
{-@ zero :: Zero @-}
zero  = 0 :: Int

{-@ one, two, three :: NonZero @-}
one   = 1 :: Int
two   = 2 :: Int
three = 3 :: Int
```

We can combine these predicates to prove non-trivial properties about arithmetic
relations, properties about data structures and pointer [memory
access](https://ucsd-progsys.github.io/liquidhaskell-tutorial/11-case-study-pointers.html#/heartbleeds-in-haskell)
to prevent bugs like Heartbleed.

```haskell
import Prelude hiding (mod, gcd)

{-@ mod :: a:Nat -> b:{v:Nat| 0 < v} -> {v:Nat | v < b} @-}
mod :: Int -> Int -> Int
mod a b
  | a < b = a
  | otherwise = mod (a - b) b

{-@ gcd :: a:Nat -> b:{v:Nat | v < a} -> Int @-}
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)
```

LiquidHaskell has developed a lovely and approachable [set of documentation](https://ucsd-progsys.github.io/liquidhaskell-tutorial/02-logic.html)
this year, and is now quite usable in production.

#### Linear Types

Intuitively linear types model finite resources by a notion of *consumption* of
variables in scope. A linear variable needs to be consumed in the same concept
it was introduced and can not be "duplicated" or "destroyed".  In a linear
λ-calculus we have typing rules  in which statements such as “this function will
use its argument exactly once” can be checked which can be used to enforce
invariants on resource usage such as memory, channels, sockets, tokens, file
descriptors, etc.

At the Haskell Exchange and ICFP there was some very early discussion about the
possibility of introducing linear types into GHC. The proposed addition would
allow weight inference and annotations to variables allowing users to constrain
variable usage. The proposal and semantics are being actively discussed on the
[issue tracker](https://ghc.haskell.org/trac/ghc/wiki/LinearTypes#Semantics).
An implementation likely remains in the far future.

An undergraduate project at Chalmers presented a simple prototype Haskell-like
language [Lollipop](https://github.com/m0ar/lollipop) using linear typing.

Jeff Polokow presented [a construction](http://functorial.com/Embedding-a-Full-Linear-Lambda-Calculus-in-Haskell/linearlam.pdf)
by which a full linear lambda calculus can be deeply embedded in the current
type system using advanced type features.

#### Backpack

Backpack is Haskell's partial answer to "module problem", of having
cross-package libraries which are currently duplicated to provide similar
interfaces. Backpack will allow libraries which are parametrized by signatures,
letting users decide how to instantiate them at a later point in time.

This work is still very early, but Edward Yang gave a insightful talk at NYC
[Haksell Meetup](https://www.youtube.com/watch?v=s_geh6SlGuM) on the current
design decisions taken in the prototype implementation. There is also a thorough
description of the applications of Backpack to solve the problem of reusable
[string
libraries](http://blog.ezyang.com/2016/09/the-base-of-a-string-theory-for-haskell/)
that work over different underlying string representations.

In the early prototype we can construct an abstract ``Str`` signature module
which provides a set of types:

```haskell
signature Str where

  data Str
  data Elem

  instance Eq Str
  instance Ord Str
  instance Show Str
  instance Monoid Str

  empty :: Str
  length :: Str -> Int
```

This is then exposed  in the cabal file for the library in the ``signatures``
field.

```haskell
name: str-sig-major1

library
  signatures:          Str
  build-depends:       base >= 4.9 && < 4.10,
                       deepseq
  default-language:    Haskell2010
```

An implementation of this signature then imports then library, and  can then be
implement the interface downstream by using a ``mixin``.

```haskell
name: str-text

library
  mixins:
    str-text (Str.Text as Str)
  build-depends:
    base,
    str-sig-major1
```

The concrete implementation of the module would look like the following:

```haskell
module Str.Text (
  Str,
  Elem,
  empty,
  length,
) where

import qualified Data.Text as T

type Str = T.Text
type Elem = Char

empty :: Str
empty = T.empty

length :: Str -> Int
length = T.empty
```

The work is still early, so this precise form may or may not end up in the final
release of Backpack.

#### Databases

Earlier this year I commented that compromises in type-safe database libraries
consisted of the following traedoffs:

> Haskell SQL libraries: endless boilerplate, opaque metaprogramming, or wall of
> 15 language exts and no inference. Pick two.

A lot of industrial Haskell sadly still uses string interpolation for SQL
synthesis, or fall back on libraries that use TemplateHaskell to unsafely lock a
specific build to a snapshot of a database at compile-time. Both of these
trade-offs are suboptimal and fall short of what we can do in other robust
frameworks like SQLAlchemy in Python. There's three different key points that
need to be addressed:

1. Definition of a schema tied to a specific databases and migration handling
   that can done at runtime.
1. Consistency checking of schema against definitions of SQL statements with
   type-safety.
1. Composition and reuse of units of SQL statements and values that preserve
   type safety under composition.

The challenge I continue to pose, that occurs quite frequently in data
warehousing and ETL, is given a dynamic (i.e. at runtime) specification of a
table in some embedding of DDL: create a table, query it, and dump the results
into an existing table of the same schema. Doing this in a safe manor requires a
rather complex [reflection
framework](http://docs.sqlalchemy.org/en/latest/core/reflection.html) by which a
table object can load the ``information_schema`` and check the constancy of the
query before evaluation.

I don't think the ideal library is quite there yet, but year I was most
impressed by Jake Wheat's very early
[``hssqlppp``](https://jakewheat.github.io/hssqlppp/0.6.0) which implements a
restricted domain language for modeling and typechecking a Postgres SQL subset
and generating queries. The full integration with a runtime SQL type-checker is
a [lovely piece of
work](https://jakewheat.github.io/hssqlppp/latest/TypeCheckTests.html) and it
would be follow naturally to lift this into a type-safe quasiquoter which would
give the ideal solution of unifying both the dynamic and static semantics of SQL
program synthesis at runtime and compile-time.

#### Industrial Users

Facebook quietly retains and recruits some of the legendary-level Haskell
talent. See: [Haskell in the Datacentre](https://simonmar.github.io/posts/2016-12-08-Haskell-in-the-datacentre.html)

Barclay's in the UK is also quietly building up an impressive team. See: [Full-time
Haskell jobs in London, at Barclays](https://neilmitchell.blogspot.co.uk/2016/09/full-time-haskell-jobs-in-london-at.html)

JP Morgan funded development on a transaction modeling language
[Hopper](https://github.com/hopper-lang/hopper-v0).

Ambiata presented at ICFP this year about a new query language
[Icicle](https://github.com/ambiata/icicle) for streaming time series, with a
[novel approach to fusion](http://conf.researchr.org/event/icfp-2016/fhpc-2016-papers-icicle-write-once-run-once)
and [released source code](https://github.com/ambiata/icicle).

Awake Networks is building a next generation network security and analytics
platform, utilizing using Purescript and Haskell.

Google has been pushing out Haskell bindings to some of their core
infastructure, including Tensorflow. See: [Haskell
Tensorflow](https://github.com/tensorflow/haskell)

There's a fairly large group of Haskellers very quietly working on building
smart contract solutions based on functional programming and formal methods. A
lot of is happening behind doors at banks but some truly amazing work is being
done in this space by singularly talented people. Watch for some amazing
future-tech to drop in 2017.

Lots of exciting things going on at Barclay's, Facebook, Target, Ambiata, Tweag,
Takt, Zalora, Galois, JP Morgan, Helium, Silk, Lumi Guide, Awake Networks,
FrontRow, Clearmatics, Standard Chartered, Digital Asset Holdings and Microsoft.

#### Stephen

Since last year I've been criss-crossing the globe through Germany, London,
Paris, San Francisco, Zurich, Portland, New York and Boston doing some mix of
advising, consulting, and investments.  Late this year, I finally ended up
starting a new company, which I'll speak about publicly in the next few
months.

I'm especially grateful for the warmth and kindness I felt traveling across
European programming scene. I consistently always had a friend in whatever city
I visited and it's been a pleasure meeting many of you. Wish you all Merry
Christmas and a Functional New Year!
