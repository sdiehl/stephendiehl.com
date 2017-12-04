---
title: Reflecting on Haskell in 2017
date: November 23, 2017
---

### Reflecting on Haskell in 2017

Alas, another year has come and gone. It feels like just yesterday I was writing
the last reflection blog post on my flight back to Boston for Christmas. I've
spent most of the last year traveling and working in Europe, meeting a lot of
new Haskellers and putting a lot of faces to names.

Haskell has had a great year and 2017 was defined by vast quantities of new
code, including 14,000 new [Haskell projects](https://github.com/search?utf8=%E2%9C%93&q=language%3Ahaskell+created%3A%3E2017-01-01&type=) on Github .
The amount of writing this year was voluminous and my list of interesting work
is eight times as large as last year. At least seven new companies came into
existence and many existing firms unexpectedly dropped large open source Haskell
projects into the public sphere. Driven by a lot of software catastrophes, the
intersection of security, software correctness and formal methods have been
become quite an active area of investment and research across both industry and
academia. It's really never been an easier and more exciting time to be
programming professionally in the world's most advanced (yet usable) statically
typed language.

Per what I guess is now a tradition, I will write my end of year retrospective
on my highlights of what happened in the Haskell scene in retrospect.

#### Writing

The amount of Haskell writing this year was vast and it's impossible to
enumerate all of the excellent writing that occurred. Some of the truly
exceptional bits that were drafted in 2017 included:

* [Old Graphs From New Types](https://blogs.ncl.ac.uk/andreymokhov/old-graphs-from-new-types/) by Andrey Mokhov
* [Type Tac Toe](http://chrispenner.ca/posts/type-tac-toe) by Chris Penner
* [Typing the Technical Interview](https://aphyr.com/posts/342-typing-the-technical-interview) by Kyle Kingsbury
* [Compiling to Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf) by Conal Elliott
* [Inlining and Specialisation](https://mpickering.github.io/posts/2017-03-20-inlining-and-specialisation.html) by Matthew Pickering
* [Counterexamples of Typeclasses](http://blog.functorial.com/posts/2015-12-06-Counterexamples.html?repost) by Phil Freeman
* [An Informal Guide to Better Compiler Errors](https://github.com/jaspervdj/talks/blob/master/2017-skillsmatter-errors/slides.md) by Jasper Van der Jeugt
* [A Tutorial on Higher Order Unification](https://github.com/jozefg/higher-order-unification/blob/master/explanation.md) by Danny Gratzer
* [On Competing with C Using Haskell](https://two-wrongs.com/on-competing-with-c-using-haskell.html) by Chris (kqr)
* [Making Movie Monad](https://lettier.github.io/posts/2016-08-15-making-movie-monad.html) by David Lettier   
* [Avoid Overlapping Instances With Closed Type Families](https://kseo.github.io//posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html) by Kwang Yul Seo
* [Writing a Formally Verified Browser in Haskell](http://www.michaelburge.us/2017/08/25/writing-a-formally-verified-porn-browser-in-coq.html) by Michael Burge
* [GHC Generics Explained](https://www.stackbuilders.com/tutorials/haskell/generics/) by Mark Karpov
* [REST API in Haskell](https://maciek.io/rest-api-in-haskell/) by Maciej Spychała
* [I Haskell a Git](http://vaibhavsagar.com/blog/2017/08/13/i-haskell-a-git/) by Vaibhav Sagar
* [Why prove programs equivalent when your compiler can do that for you?](http://www.joachim-breitner.de/blog/717-Why_prove_programs_equivalent_when_your_compiler_can_do_that_for_you_) by Joachim Breitner
* [Free Monads Considered Harmful](https://markkarpov.com/post/free-monad-considered-harmful.html) by Mark Karpov
* [Beautiful Aggregations with Haskell](http://tech.frontrowed.com/2017/09/22/aggregations/) by Evan Borden
* [Submitting Haskell Functions to Z3](http://newartisans.com/2017/04/haskell-and-z3/) by John Wiegley

#### Books & Courses

Vladislav Zavialov and Artyom Kazak set out to write a book on the 
netherlands of Intermediate Haskell development, a mythical place that we all
seemingly pass through but never speak of again. The project is intuitively called
[Intermediate Haskell](https://intermediatehaskell.com/) and is slated to be
released in 2018

Bartosz Milewski finished writing [Category Theory for
Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)
which is freely available and also generated as
[PDF](https://github.com/hmemcpy/milewski-ctfp-pdf/releases/download/v0.4/category-theory-for-programmers.pdf).

Brent Yorgey drafted a new course for teaching introduction to Computer Science
using Haksell at [Hendrix
University](http://ozark.hendrix.edu/~yorgey/360/f16/). Dmitry Kovanikov and
Arseniy Seroka also drafted a course for a wide variety of [intermediate to
advanced Haskell topics](https://github.com/jagajaga/FP-Course-ITMO) at ITMO
university. Some of which are in Russian but nevertheless большое письмо!

#### GHC

The Glorious Glasgow Haskell Compiler had it's 8.2 release, and landed to much
rejoicing. Major features such as unboxed sum types landed as planned in GHC
8.2. There were many longstanding issues that were closed and many miscellaneous
fixes contributed in 2017. For instance GHC now uses alternative linkers such as
`ld.gold` or `ld.lld` instead of the system default ld.

Semigroup is now a [superclass of Monoid](
https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid).

There was quite a bit of work on GHC [cross
compilation](https://medium.com/@zw3rk) to ARM. The new build system
[Hadrian](https://github.com/snowleopard/hadrian) has been in work for the past
three years, has was finally been merged into the GHC tree.

The DevOps Group has officially started and is being funded to help maintain the
infrastructure used to host Haskell packages and build GHC. The general push of
the group has been toward using hosted CI services, Appveyor and CircleCI and a
greater use of more transparent platforms such as Github for GHC development.

There is work on a major refactor of the AST types to use the new [Trees that
Grow](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow) research
to allow GHC API user to extend the AST for their own purposes,Eventually this
may allow the split of the AST types out of the ghc package, allowing tooling
authors, Template Haskell users, and the compiler itself to use the same AST
representation.

GHC is partially accepting pull requests on
[Github](https://github.com/ghc/ghc/pull/14) although most of the development
still occurs on the mailing list and Phabricator.

There was significant engineering effort to allow GHC to produce [deterministic
build artifacts](https://ghc.haskell.org/trac/ghc/wiki/DeterministicBuilds) to
play nicely with caching reproducible build systems such as Buck and Bazel.
Previously the emitted ``.hi`` files would contain non-deterministic data such
as hashes, timestamps and unique name supply counts.

**Errors**

GHC 8.2 added wonderful new colorful error messages with caret diagnostics for
syntax and type errors:

<img src="/images/ghc_errors.jpg" style="max-width: 100%" alt="Colorful errors GHC 8.2"/>

**Compact Regions**

Support for ‘Compact Regions’ landed in GHC 8.2. Compact regions are manually
allocated regions where the data allocated inside it are compacted and not
traversed by the GC. This is amenable for long-lived data structures that are
resident in memory without mutation frequently occurring. 

The interface can be accessed through the
[ghc-compact](https://hackage.haskell.org/package/ghc-compact) modules and used
to create compact malloc'd memory.

```haskell
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Compact

main = compact (B.pack ['a'..'c'])
```

The [test suite](https://github.com/shlevy/ghc/tree/f1dfce1cb2a823696d6d3a9ea41c2bc73d949f12/libraries/compact/tests)
inside GHC maintains the best illustrations of it's use for complex non-traditional data structures.

**Type.Reflection**

GHC added a new more expressive Typeable mechanism using the
[``Type.Reflection``](https://downloads.haskell.org/~ghc/master/libraries/base/base/Type-Reflection.html)
module. ``typeRep`` can be applied with explicit type applications to arrows and
functions can checked for type-level equality after application.

```haskell
λ> typeRep @(->)
(->) 'LiftedRep 'LiftedRep
```

```haskell
λ> typeRep @(Maybe Int) == App (typeRep @Maybe) (typeRep @Int)
True
```

**Coercible**

Not new to GHC 8.2 although the base library now exposes the `Coercible`
constraints allowing polymorphism over types which have the same runtime
representations to be safely coerced at runtime. This can also be extended to
polymorphic functions which take a compile-time proof of the equivalence of two
runtime data layouts.

```haskell
import Data.Coerce

newtype Meters = Meters Integer deriving (Num)
newtype Feet = Feet Integer deriving (Num)

f :: (Coercible a b, Num b) => a -> b -> b
f x y = coerce x + y
```

```haskell
λ> f (Meters 3) 4
7
λ> f (Feet 3) 4
7
λ> f 4 (Feet 3)
Feet 7 
```

GHC tracks the usage (i.e. role) of type variables used as parameters as either
`nominal` `representational` or `phantom` allowing types that differ only in a
phantom type of nominal parameters to be safely coerced.

**Join Points**

Luke Maurer and Simon Peyton Jones merged new work on [join
points](https://ghc.haskell.org/trac/ghc/wiki/SequentCore) which modifies GHC
Core to optimize for join points in code. In Core, a *join point* is a specially
tagged function whose only occurrences are saturated tail calls. In the actual
GHC Core AST, a join point is simple bit of metadata indicated by `IdDetails` of
the binder

Simon Peyton Jones presented the keynote at Haskell Exchange on his
collaboration on [Compiling without
Continuation](https://skillsmatter.com/skillscasts/9182-keynote-compiling-without-continuations)
which present the ideas and core optimizations that are allowed by the new join
points.

**Deriving Strategies**

In GHC 8.0 there were two alternative methods for automatic deriving of
typeclass instances, using `GeneralizedNewtypeDeriving` and `DeriveAnyClass`. In
addition there was also the wired-in deriving mechanisms for `Show`, `Read`, etc
that were hardcoded into the compiler. These all used the same syntax and would
conflict if multiple pragmas were enabled in a module.

The addition of `DerivingStrategies` allows us to disambiguate which deriving
mechanism to use for a specific instance generation.

```haskell
newtype Meters = Meters Int
  deriving stock    (Read, Show)
  deriving newtype  (Num)
  deriving anyclass (ToJSON, FromJSON)
```

#### Backpack

Edward Yang finished his PhD thesis on Backpack which was integrated in the GHC
tree.  The new branch adds support ``.bkp files``, which specify abstract
interfaces which can be instantiated in modules and used to construct Haskell
modules which work polymorphically across multiple module instantiations.

For example an abstract string type can be written which operates over a
module parameter `Str``:


```haskell
unit abstract-str where
    signature Str where
        data Str
        len :: Str -> Int

    module AStr (alen) where
        import Str

        alen :: Str -> Int
        alen = len
```

We can create (contrived) instantiations of this module for lists of ints and
chars which expose a polymorphic length function over both.

```haskell
unit str-string where
    module Str where
        type Str = String

        len :: Str -> Int
        len = length

unit str-list where
    module Str where
        type Str = [Int]

        len :: Str -> Int
        len = length
```

The modules can then be opened as speific namespaces with the exported functions
able to be called over both module types.

```haskell
unit main where
    dependency abstract-str[Str=str-string:Str] (AStr as AStr.Int)
    dependency abstract-str[Str=str-list:Str] (AStr as AStr.String)

    module Main (main) where
        import qualified AStr.Int
        import qualified AStr.String

        main :: IO ()
        main = do
            print $ AbstractStr.String.alen "Hello world"
            print $ AbstractStr.Int.alen [1, 2, 3]
```


With the latest GHC this can be compiled with the ``--backpack`` which generates
the sum of all the ``hi`` files specified in the ``.bkp`` file and resolves
values at link-time.

```bash
$ stack exec -- ghc --backpack example.bkp
```

While the functionality exists today, I'm not aware of any large projects using
Backpack. Bolting this functionality onto an ecosystem that has spent a decade
routing around many of the problems this system aims to solve, poses a huge
engineering cost and may take a while to crystallize.

#### Summer of Haskell

Google lacked vision this year and did not sponsor the Haskell Organization for
Summer of Code. But the program proceeded regardless with private sponsorship
from industrial users. Fifteen students were paired with mentors and many
successful projects and [collaborations
resulted](https://summer.haskell.org/news/2017-09-15-final-results.html).

#### LLVM

The LLVM bindings for Haskell saw quite a bit of work this year and were forked
into a new organization [llvm-hs](https://www.github.com/llvm-hs) and added
support for LLVM 4.0 - 5.1:

1. [llvm-hs](https://github.com/llvm-hs/llvm-hs/tree/llvm-5/llvm-hs)
1. [llvm-hs-pure](https://github.com/llvm-hs/llvm-hs/tree/llvm-5/llvm-hs-pure)
1. [llvm-hs-pretty](https://github.com/llvm-hs/llvm-hs-pretty/)

In a collaboration with Joachim Breitner and myself at Zurihac a type-safe LLVM
library which embeds the semantics of LLVM instructions into the Haskell
[type-system](https://github.com/llvm-hs/llvm-hs-typed) was written.

Siddharth Brat started work on an STG to LLVM backend
[simplehxc](https://pixel-druid.com/blog/announcing-simplexhc/) before doing a
rewrite in C++.

Moritz Angermann has been continuing to develop a Haskell library for emitting
and reading [LLVM Bitcode format](https://github.com/angerman/data-bitcode-llvm)
as well as work on the [llvm-ng](https://ghc.haskell.org/trac/ghc/ticket/10074)
backend which is a major rewrite of the GHC LLVM code generator.

#### Linear Types

Arnaud Spiwack prototyped a [extension of
GHC](https://github.com/tweag/linear-types) which augments the type system with
linear types. [Edsko de
Vries](http://edsko.net/2017/01/08/linearity-in-haskell/) wrote a detailed blog
post about the nature of linearity and it's uses.

The proposal extends the typing of functions to include linearity constraints on
arrows, enforcing that variables or references are created and consumed with
constrained reference counts. This allows us to statically enforce reference
borrowing and allocations in the typechecker potentially allowing us to enforce
lifetime constraints on closures and eliminating long-lived memory from being
used with constructed unbounded lifetimes, thereby eliminating garbage
collection for some Haskell functions.

For instance use of the linear arrow ``(a ->. b)`` can enrich the existing raw
memory access functions enforcing the matching of allocation and free commands
statically. The multiplicity of usage is either ``0``, ``1`` or ``ω`` and the
linear arrow is syntatic sugar for unit multiplicity are aliases for ``(:'1
->)``.

```haskell
malloc :: Storable a => a ->. (Ptr a ->. Unrestricted b) ->. Unrestricted b
read :: Storable a => Ptr a ->. (Ptr a, a)
free :: Ptr a ->. ()
```

New abstractions such as movable, consumable and dupable references can be
constructed out of existing class hierarchies and enriched with static linearity
checks:

```haskell
class Consumable a where
  consume :: a ->. ()

class Consumable a => Dupable a where
  dup :: a ->. (a, a)

class Dupable a => Movable a where
  move :: a ->. Unrestricted a

instance Dupable a => Dupable [a] where
  dup [] = ([], [])
  dup (a:l) = shuffle (dup a) (dup l)
    where
      shuffle :: (a, a) ->. ([a], [a]) ->. ([a], [a])
      shuffle (a, a') (l, l') = (a:l, a':l')

instance Consumable a => Consumable [a] where
  consume [] = ()
  consume (a:l) = consume a `lseq` consume l
```

As part of the Summer of Haskell Edvard Hübinette used linear types to construct
a safer stream processing library which can enforce resource consumption
statically [using linearity](https://github.com/m0ar/safe-streaming).

There is considerable debate [about the
proposal](https://github.com/ghc-proposals/ghc-proposals/pull/91) and the nature
of integration of linear types into GHC. With some community involvement these
patches could be integrated quite quickly in GHC.

#### LiquidHaskell

[LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell-blog/) the large
sutie of tools for adding refinement types to GHC Haskell continued development
and became considerably more polished. At HaskellExchange several companies were
using it in anger in production. For example, we can enforce statically the
lists given at compile-time statically cannot contain certain values by
constructing a proposition function in a (subset) of Haskell which can refine
other definitions:

```haskell
measure hasZero :: [Int] -> Prop
hasZero [] = false
hasZero (x:xs) = x == 0 || (hasZero xs)

type HasZero = {v : [Int] | (hasZero v)}

-- Accepted
xs :: HasZero
xs = [1,2,3,4]

-- Rejected
ys :: HasZero
ys = [0,1,2,3]
```

This can be used to [statically
enforce](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Data/Maybe.spec)
that logic that consumes only a Just value can provably only be called with a
``Just`` with a ``isJust`` measure:

```haskell
measure isJust :: forall a. Data.Maybe.Maybe a -> Bool
isJust (Data.Maybe.Just x)  = true 
isJust (Data.Maybe.Nothing) = false 
```

This year saw the addition of inductive predicates allowing more complex
properties about non-arithmetic refinements to be checked. Including properties
about lists 

```haskell
measure len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + (len xs)

-- Spec for Data.List exports refined types which can be statically refined with
-- length constraints.
[] :: {v:[a]| len v = 0}
(:) :: _ -> xs:_ -> {v:[a]| len v = 1 + len xs}

append :: xs:[a] -> ys:[a] -> {v:[a]| len v = len xs + len ys}
```

The full library of specifications is now quite extensive and adding
[LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include)
to an existing codebase is pretty seamelss

#### Foundation

Foundation is an alternative Prelude informed by modern design practices and
data structures. It ships a much more sensible and efficient packed array of
UTF8 points as it's default [`String`](https://hackage.haskell.org/package/foundation-0.0.17/docs/Foundation-String.html)
type. Rethinks the `Num` [numerical tower](https://hackage.haskell.org/package/foundation-0.0.17/docs/Foundation-Numerical.html) , and statically distinguishes [partial functions](https://hackage.haskell.org/package/foundation-0.0.17/docs/Foundation.html#t:Partial).
Also has fledgling documentation beyond just

Last year Foundation was a bit early, but this year at Zurihac several companies
in London reported using it fully in production as a full industrial focused
Prelude.

#### Editor Tooling

Editor integration improved, adding best in modern tooling to most of the
common editors: 

+----------------------+-----------------------------------------------------------------------+
| Editor               | Haskell Integration                                                   |
+======================+=======================================================================+
| **Atom**             | https://atom.io/packages/ide-haskell                                  |
+----------------------+-----------------------------------------------------------------------+
| **Emacs**            | https://commercialhaskell.github.io/intero/                           |
+----------------------+-----------------------------------------------------------------------+
| **IntelliJ**         | https://plugins.jetbrains.com/plugin/8258-intellij-haskell            |
+----------------------+-----------------------------------------------------------------------+
| **VSCode**           | https://marketplace.visualstudio.com/items?itemName=Vans.haskero      |
+----------------------+-----------------------------------------------------------------------+
| **Sublime**          | https://packagecontrol.io/packages/SublimeHaskell                     |
+----------------------+-----------------------------------------------------------------------+

Monica Lent wrote a lovely post on the state of art in [Vim and Haskell
integration](http://monicalent.com/blog/2017/11/19/haskell-in-vim/).

Rik van der Kleij has done an impressive amount of work adapting the IntellJ IDE
to [work with Haskell](https://github.com/rikvdkleij/intellij-haskell).
Including a custom parser handling all of the syntax extensions, name lookup,
Intero integration and integration with haskell-tools refactoring framework.

Development on the
[haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) has picked
up again in the last few months.

#### Formal Methods

The [DeepSpec](https://deepspec.org/page/Research/), a collaboration between
MIT, UPenn, Princeton and Yale, is working on a network of specification that
span many compilers, languages and intermediate representations with the goal of
achieving full functional correctness of software and hardware. Both Haskell and
LLVM are part of this network of specifications. The group has successfully
written a new formal calculus describing the GHC core language and proved it
type sound in Coq. The project is called
[corespec](https://github.com/sweirich/corespec) and is described in the paper
"A Specification for Dependent Types in Haskell".

In addition the group also published a paper “Total Haskell is Reasonable Coq”
and provided a utlity [hs-to-coq](https://github.com/antalsz/hs-to-coq) which
converts haskell code to equivalent Coq code.

The Galois group started [formalizing the
semantics](https://github.com/GaloisInc/cryptol-semantics) of Cryptol, a
compiler for high-assurance cryptographic protocols which is itself
written in Haskell.

Michael Burge wrote a cheeky article about extracting a specification for a
["domain specific"
browser](http://www.michaelburge.us/2017/08/25/writing-a-formally-verified-porn-browser-in-coq.html)
from Coq into Haskell.

#### Pragma Proflieration & Prelude

Writing Haskell is almost trivial in practice. You just start with the magic
fifty line `{-# LANGUAGE ... #-}` incantation to fast-forward to 2017, add
then add  150 libraries that you've blessed by trial and error to your cabal
file, and then just write down a single type signature whose inhabitant is
`Refl` to your program. In fact if your program is longer than your import list
you're clearly doing Haskell all wrong.

In all seriousness, Haskell is not the small language it once was in 1998, it's
reach spans many industries, hobbyists, academia and many classes of people with
different incentives and whose ideas about the future of the language are
mutually incompatible. Realistically the reason why the Prelude and extension
situation aren't going to change anytime soon is that no one person or company
has the economic means to champion such a change. It would be enormously
expensive and any solution will never satisfy everyone's concerns and desires.
*Consensus is expensive*, while making everything opt-in is relatively cheap.
This is ultimately the equilibrium we've converged on and baring some large sea
change the language is going to remain in this equilibrium.

#### Haskell Survey

Taylor Fausak conducted an [unofficial
survey](http://taylor.fausak.me/2017/11/15/2017-state-of-haskell-survey-results/)
of Haskell users with some surprising results about widespread use of Haskell.
Surprisingly there are reportedly 100 or more people who maintain 100,000 or
more lines of Haskell code. Not so surprisingly most people have migrated to
Stack while vim and emacs are the editors of choice.

While the majority of respondents are satisfied with Haskell the language the
response are somewhat mixed the quality of libraries and the bulk of respondents
reported Haskell libraries being *undocumented* , *hard to use* *hard
to find*, and don't *integrate well*. 

#### Projects

Idris, the experimental dependently typed language, reached 1.0 release and
became one of the larger languages which is itself written in Haskell.

The most prolific Haskell library Pandoc release it's version 2.0.

Several other groups published new compilers in the Haksell-family of languages.
Intel finally open sourced the [Intell Haskell
compiler](https://github.com/IntelLabs/flrc) which was a research project in
more optimal compilation techniques. Morgan Stanley also released
[Hobbes](http://lambda-the-ultimate.org/node/5452) a Haskell-like language used
internally at the bank featuring several novel extensions to row-types and C++
FFI integration. A prototype Haskell compiler was also written in
[Rust](https://github.com/Marwes/haskell-compiler).

The SMT solver integration library SBV saw a major rewrite of it's internal and
it's old [tactics
system](https://hackage.haskell.org/package/sbv-7.3/docs/Data-SBV-Control.html).
The library is heavily used in various projects as an interface to Z3 and CVC4
solvers.

Uber released a library for parsing and analysis of Vertica, Hive, and Presto
[SQL queries](https://github.com/uber/queryparser).

Wire released the [backend service]( https://github.com/wireapp/wire-server) to
their commercial offering.

The Advanced Telematic Systems group in Berlin released a Quickcheck family
library for doing for property testing of models about [state
machines](https://github.com/advancedtelematic/quickcheck-state-machine).  Bose
also released Smudge a tool for doing developemtn and [analysis of large state
machines](https://github.com/Bose/Smudge) for hardware testing.

Florian Knupfer released a new high-performance HTML [combinator
library](https://github.com/knupfer/type-of-html) for templating.

Galois continued with HalVM unikernel continued development this year, and
several [HalVM Docker
Imagaes](https://github.com/GaloisInc/HaLVM/wiki/Using-Docker-and-the-HaLVM)
were published allowing a very convenient way to write and test code against
HalVM.

Facebook released a Haskell string parsing library
[duckling](https://github.com/facebook/duckling) which parses human input into a
restricted set of semantically tagged data. Facebook also prototyped a technique
for hot-swapping [Haskell code at runtime](https://simonmar.github.io/posts/2017-10-17-hotswapping-haskell.html)
using clever GHC API trickery.

Chris Done released [vado](https://github.com/chrisdone/vado) a browser engine
written in Haskell.

Jonas Carpay released [apecs](https://github.com/jonascarpay/apecs) an entity
component system for game development in Haskell.

Csaba Hruska continued work on a [GRIN
compiler](https://github.com/csabahruska/grin) and code generator, an
alternative core language for compiling lazy functional languages based on the
work by Urban Boquist.

Dima Szamozvancev released [mezzo](https://github.com/DimaSamoz) a library and
embedded domain-specific language for music description that can enforce rules
of compositionality of music statically and prevent bad music from being
unleashed on the world.

Conal Elliot and John Wiegley advanced a novel set of ideas on [Compiling with
Categories](https://github.com/conal/concat) which allows the bidirectional
representation of Haskell functions as categorical structures. Although
currently implemented as a GHC typechecker extension it is a promising research
area.

Harry Clarke published a paper on [Generics Layout-Preserving
Refactoring](https://www.cs.kent.ac.uk/people/staff/dao7/publ/reprinter2017.pdf)
using a [reprinter](https://github.com/camfort/reprinter). i.e. a tool which
takes a syntax tree, the original source file, and produces an updated source
file which preserves secondary notation.  This can be used to build tools like
haskell-tools for arbitrary languages and write complicated refactoring suites.

Joachim Breitner contributed a prolific amount of open source projects including
a new technique (ghc-proofs) for proving the equivalence of Haskell programs
using a GHC plugin, (veggies) a verified simple LLVM code generator for GHC, and
(inspection-testing) new technique for verifying properties of Core.

1. [veggies](https://github.com/nomeata/veggies)
1. [ghc-proofs](https://github.com/nomeata/ghc-proofs)
1. [inspecction-testing](https://github.com/nomeata/inspection-testing)

The [ghc-proofs plugin](https://www.youtube.com/watch?v=jcL4bp4FMUw&feature=youtu.be) allows us
to embed equation and relations into our Haskell module and potentially allow
GHC to prove them correct by doing symbolic evaluation and evaluating them as
far as possible and checking the equational relations of the equations sides.
Right now it works for contrived and simple examples, but is quite a interesting
approach that may yield further fruit.

```haskell
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module Simple where

import GHC.Proof
import Data.Maybe

my_proof1 = (\f x -> isNothing (fmap f x))
        === (\f x -> isNothing x)
```

```bash
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
GHC.Proof: Proving my_proof1 …
GHC.Proof proved 1 equalities
```

#### Haddock

Haddock is creaking at the seams. Most large Haskell projects (GHC, Stack, Agda,
Cabal, Idris, etc) no longer use it for documentation. The codebase is dated and
long standing issue like dealing with reexported modules are still open.

There is a large vacuum for a better solution to emerge and compatibility with
RestructuredText would allow easy migration of existing documentation.

#### Databases

This year saw two new approaches to Haskell database integration:

1. [Selda](https://github.com/valderman/selda) - A library interacting with relational databases inspired by LINQ and Opaleye.
1. [Squel](https://github.com/morphismtech/squeal) -  A deep embedding of PostgreSQL in Haskell using generics-sop.

This still remains an area in the ecosystem where many solutions end up
needlessly falling back to TemplateHaskell to generate data structures and
typically tend to be a pain point. Both these libraries use generics and
advanced type system features to statically enforce well-formed SQL query
construction.

#### Data Science & Numerical Computing

Chris Doran presented a concise interpretation of Geometric Algebra, a
generalization of linear algebra in terms of graded algebra [written in
Haskell](https://github.com/ga/Haskell/blob/master/GABlade.hs). A talk on this
project was presented at the [Haskell
Exchange](https://skillsmatter.com/conferences/8522-haskell-exchange-2017#program).

At ZuriHac several people expressed interest in forming a [Data
Haskell](https://github.com/DataHaskell/) organization to work on advancing the
state of Haskell libraries for data science and macing learning. There is
considerable interest of the constructing the right abstractions for a
well-typed dataframe library.

Tom Nielson is furiously working on a
[suite](https://github.com/diffusionkinetics/open) of of projects, mostly
related to data science, machine learning and statistics in Haskell.

The [Accelerate project](https://github.com/Accelerate) has been seeing a lot of
development work recently and now has support for latest versions of LLVM and
CUDA through llvm-hs project.

A group at the University of Gothenburg released
[TypedFlow](https://github.com/GU-CLASP/TypedFlow) a statically typed
higher-order frontend to TensorFlow.

#### Eta

Typelead continued working on a Java Virtual Machine backend to GHC called
[Eta](http://eta-lang.org/).  The project has seen considerable amount of
person-hours invested in compatibility and the firm has raised capital to pursue
the project as a [commercial venture](https://www.crunchbase.com/organization/typelead).

The project does seem to be diverging from the Haskell mainline language in both
syntax and semantics which raises some interesting questions about viability.
Although the Haskell language standard seems to be stuck and not moving forward,
so there's an interesting conundrum faced by those trying to build on top of GHC
in the long run.

#### WebAssembly

WebAssembly has been released and is supported by all major browsers as of
December 2017. This is been piquing the interest of quite a few Haskellers
(including myself) who have been looking for a saner way to interact with
browsers than the ugly hack of ejecting hundreds of thousands of lines of
generated Javascript source. WebAssembly in either standalone projects or as a
target from GHC's intermediate form STG to the browser offers an interesting
path forward.

1. [Haskell WASM](https://github.com/haskell-wasm/)
2. [WebGHC](https://github.com/WebGHC)
3. [ministgwasm](https://github.com/neuromancer42/ministgwasm)
4. [forest-lang](https://github.com/forest-lang/core)

#### Industry

An incomplete list of non-consulting companies who are actively using Haskell or
have published Haskell open sources libraries follows:

+----------------------+
| Product Companies    |
+======================+
| Standard Chartered   |
+----------------------+
| Galois               |
+----------------------+
| Intel                |
+----------------------+
| Target               |
+----------------------+
| Uber                 |
+----------------------+
| Vente Privee         |
+----------------------+
| FrontRow             |
+----------------------+
| NStack               |
+----------------------+
| Morgan Stanley       |
+----------------------+
| Takt                 |
+----------------------+
| Fugue                |
+----------------------+
| Habito               |
+----------------------+
| Asahi Net            |
+----------------------+
| Sentenai             |
+----------------------+
| IOHK                 |
+----------------------+
| Awake Networks       |
+----------------------+
| Facebook             |
+----------------------+
| Adjoint              |
+----------------------+
| DigitalAsset         |
+----------------------+
| AlphaSheets          |
+----------------------+
| Channable            |
+----------------------+
| SlamData             |
+----------------------+
| Wire                 |
+----------------------+
| JP Morgan            |
+----------------------+
| Bose                 |
+----------------------+

There are three consulting companies also supporting Haskell in industry.

+----------------------+
| Consultancies        |
+======================+
| Well-Typed           |
+----------------------+
| Tweag                |
+----------------------+
| FP Complete          |
+----------------------+

#### Conferences

Both the [Haskell
Exchange](https://www.flickr.com/photos/skillsmatter/sets/72157686979473651/)
and ZuriHac conference had record attendance this year. Haskell was
well-represented in many interdisciplinary conferences include StrangeLoop,
CurryOn, LambdaDays, LambdaConf, and YOW.

Most of the videos are freely available online.

1. [Haskell Exchange Videos](https://skillsmatter.com/conferences/8522-haskell-exchange-2017)
2. [ComposeConf Videos](https://www.youtube.com/channel/UC0pEknZxL7Q1j0Ok8qImWdQ/videos)
3. [ZuriHac Videos](https://www.youtube.com/watch?v=KzqNQMpRbac&list=PLOvRW_utVPVkoZ5GuodkejFU8MiH6_SB7)
4. [ICFP Videos](https://www.youtube.com/watch?v=MmbleUgU2Rg&list=PLnqUlCo055hW7kU-SBQEhC_87etA5Gqlq)

<img src="/images/zurihac.jpg" style="width: 100%" alt="Haskellers by the lake in Rapperswil, Switzerland"/>

In 2018 I have plans to visit Zurich, Sydney, Budapest, Amsterdam, San
Francisco, Paris, Copenhagen and Tokyo this year. Hopefully I get to meet more
Haskellers and share some ideas about the future of our beautiful language.
Until then, Merry Christmas and Happy New Haskell Year. 
