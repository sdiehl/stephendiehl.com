---
title: Haskell Poll Results
date: December 21, 2014
---

### Haskell Poll Results

I put out a call for data and comments about topics that Haskell people felt
were under represented. I'm sure I'll take some flak for the informal poll and
methodology, but I feel that having at least some concrete data about the
Haskell zeitgeist is better than nothing.

As my [wise friends](https://twitter.com/a_cowley/status/546820870329860096)
have noted, baked into the poll is a hypothesis that people will give different
responses based on which domain they use Haskell for ( i.e. compiler developers
have different focus than web developers). This seems to be confirmed by the
data, but at the same time also self-selects for people who have a narrow focus.
Also given the nature of the collection, we're only going to select people who
are willing to respond and active on Haskell forums.

The total counts include all individuals, even those who gave a "None of the
above" or write-in for their domain. These are some of the most bizarre results,
but when these points are excluded and then factored on domain the results seem
much more sensible.

Thanks again for all those who volunteered their opinions.

#### Domains

The first question concerned which domain of programming the questionee is
involved in. This field was exclusive choice so that we could bin on it when
doing statistics later. The most popular domains in order are:

1. Web Development
1. Compiler Design
1. Pure Mathematics or CS Theory
1. Data Analysis
1. Numerical Computing
1. Education
1. Financial Modeling

The number of people involved in compiler development was somewhat surprising
result to say the least. The other domains seemed to fall out fairly natural.
There were also quite a few write-ins in various forms, and many comments
indicating multidisciplinary fields. The write-ins were excluded from the later
binning on the various factors and only included in the total count.

#### Skill Level

The self-rated skill level turned out to be a fairly typical distribution with a
median of 5, mode of 5, and mean of 5.3. Curiously most Haskellers in the poll
rated themselves below 5.

This result is probably not meaningful or accurate.

#### Type System

The six most mentioned type systems features surprised me a bit, they were:

1. **Impredicative Types**
2. **Kind Polymorphism**
3. **Singletons**
4. **Rank-N Types**
6. **GADTs**
7. **Type Families**

**Impredicative Types** is a curious answer. I'm baffled why it seems so
dominant. The extension is widely considered to be broken or a misfeature, and I
don't think I've never seen it used in the wild. My only guess is that it
relates to it showing up so frequently in GHC error reporting that many people
are curious about it having never actually used it.

**Kind Polymorphism** is understandable since it's a fairly recent addition to
GHC and already there seems to a need for many poly-kinded versions of existing
structures in Base. Kind promotion itself is still a very under-used feature.

**Singletons** is also a rather fruitful modern area of research in bringing
some semblance of dependent types to Haskell. The
[singletons](http://hackage.haskell.org/package/singletons) library has been the
subject of several ICFP and meetup talks.

**Rank N-Types** invariably seems to always be a point of confusion in some
discussions. I would indeed say that higher-ranked polymorphism is not widely
understood and can be very subtle.

**Type Families** is also a fairly new feature in GHC, and the subject of much
active exploration. Only a few months ago did GHC 7.8 get closed type families,
giving us the ability to encode much more [complex
logic](http://dev.stephendiehl.com/hask/#closed-type-families) at the type level

Binned amongst the **Web Development** user group, the most mentioned topics
are:

1. Impredicative Types
2. GADTs
2. Type Families

Binned amongst the **Compiler Design** user group, the most mentioned topics
are:

1. Impredicative Types
2. Kind Polymorphism
2. Type Families

Binned amongst the **Pure Mathematics or CS Theory** user group, the most
mentioned topics are:

1. Singletons
2. Kind Polymorphism
2. Type Families

Binned amongst the **Data Analysis** user group, the most mentioned topics are:

1. Kind Polymorphism
2. Type Families
2. Rank-N Types

<p>
<img src="/images/TypeSystem_poll.png">
</p>

#### Patterns

The pattern results were also somewhat surprising as well, they were:

1. F-Algebras
2. Cont
3. GHC.Generics
4. Profunctors
5. Final Interpreters
6. Arrows

**F-Algebras** is also a puzzling response, but was overwhelming the most
mentioned response from the total count. There are some [great
articles](https://www.fpcomplete.com/user/bartosz/understanding-algebras) about
the relations between F-Algebras and catamorphisms. They are used somewhat
rarely and I'm genuinely surprised about why this answer is the most popular in
the total count.

**Continuation passing** and CPS conversion seems to be one of those thuddingly
concrete topics that seems to confuse more than it should. Continuations do
invert the way we normally think about control flow which can be confusing.

**GHC.Generics** are another topic which is indeed rather under represented, at
the time of writing this I cannot actually think of a resource to point anyone
at that explains how to use Generics beyond what the [GHC
manual](https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/generic-programming.html)
explains. At the same time Generics are incredibly powerful and useful.

**Profunctors** is understandably confusing, and puzzlingly it seems to be a
dependency of a large number of libraries on Hackage while the library itself
has limited documentation.

**Arrows** is also very understandable. They seem to have been a very active
area of research 10 or so years ago. Leaving us with a lot of half-baked
libraries around seemingly beautiful ideas, that then died out leaving us only
with hints of the possibilities of arrows. That and ``ArrowSyntax`` extensions
which are very odd and seem to be understood or used by shockingly few people.

Binned amongst the **Web Development** user group, the most mentioned topics
are:

1. van Laarhoven Lenses
2. Exception Handling
3. Template Haskell

Binned amongst the **Compiler Design** user group, the most mentioned topics
are:

1. Cont
2. Free Monads
3. Profunctors

Binned amongst the **Pure Mathematics or CS Theory** user group, the most
mentioned topics are:

1. F-Algebras
1. Cont
2. Profunctors

Binned amongst the **Data Analysis** user group, the most mentioned libraries
are:

1. Free Monads
2. van Laarhoven Lenses
3. Heterogeneous Lists

There were no write-ins for this category.

<p>
<img src="/images/Patterns_poll.png">
</p>

#### Libraries

The libraries section was admittedly a bit of a grab bag, there is no way to
poll on all of Hackage so inevitably I had to chose an arbitrary sample of
cross-domain libraries. A more exhaustive poll of all of Hackage libraries
people are interested in is something I would be interested in doing, but I'm
not sure how to do it in a methodological way.

I chose not to include web libraries since they often
tend to fall under an umbrella project ( yesod, snap, happstack ) and exhibit
some odd clustering behavior that makes them somewhat unique amongst other
packages. The top 20 packages are listed below:

1. **repa** - A numerical library for high performance, regular,
   multi-dimensional, shape polymorphic parallel arrays. 
2. **uniplate** - A generics library for traversals and rewrites.
3. **mmorph** - Monad morphisms, a utility library for working with monad
   transformers.
4. **free** - A implementation of free monads.
6. **lens-family** - A lightweight minimalistic lens library in the van
   Laarhoven style.
7. **unbound** - A binder library for capture avoiding substitution for building
   type checkers and interpreters.
8. **operational** - A monadic utility library for building complex monadic
   control flow.
9. **pipes** - A coroutine streaming library with strong categorical
   foundations.
10. **parsec** - A parser combinator library.
11. **esqueleto** - A SQL query embedded DSL.
12. **safe** - A utility library providing total function variants for many
    Prelude partial functions.
13. **accelerate** - A numerical library for parallel array computing with
    various backends.
14. **resourcet** - Deterministic allocation and freeing of scarce resources
15. **fgl** - Functional graph theory library.
16. **optparse-applicative** - Command line option parsing.
17. **quickcheck** - Property based testing framework.
18. **hakyll** - Static website generator.
19. **vector** - Generic computing library providing boxed and unboxed
    contigious memory arrays and fusion.
20. **llvm-general** - Bindings to the LLVM code generation and compiler
    framework.
21. **diagrams** - Drawing library and embedded domain language for vector
    graphics.

Binned amongst the **Web Development** user group, the most mentioned libraries
are:

1. pipes
2. esqueleto
3. mmorph

Binned amongst the **Compiler Design** user group, the most mentioned libraries
are:

1. uniplate
2. graphscc
3. llvm-general

Binned amongst the **Pure Mathematics or CS Theory** user group, the most mentioned libraries
are:

1. repa
2. uniplate
3. free

Binned amongst the **Data Analysis** user group, the most mentioned libraries
are:

1. repa
2. accelerate
3. lens-family

The most popular write-ins were:

1. reactive
1. uu-parsinglib
1. lambdacube-gl
1. trifecta
1. machines
1. recursion-schemes

Most of these results are self explanatory and reflect my intuition about
Hackage as well. There are some weird anomalies though:

**Repa** seemingly has a large amount of
[tutorials](https://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial)
and [worked examples](https://hackage.haskell.org/package/repa-examples) so this
result has me scratching my head a little bit.

On a personal note, I'm somewhat saddened by how often **llvm-general** shows up
given how much time I spent on, what I thought, was a [very extensive
tutorial](http://www.stephendiehl.com/llvm/) on the subject.

<p>
<img src="/images/Libraries_poll.png">
</p>

> A lot of people don't understand the difference between `mtl` and `transformers` and think `mtl` is the only way to do monad transformers

> hsc3, the supercollider library, could really use some documentation IMO.  One can rely on the supercollider docs, but that's an extra layer of lookup, and you have to infer the meaning of arguments that don't always correspond exactly.  Seems to me sound is one area where new programmers might be interested in playing with haskell, unfortunately its not too noob friendly.  

#### Language Features

For language features I tried to poll on topics specific to GHC's implementation
details. The results were overwhelmingly about performance and profiling:

1. Profiling Memory
2. Rewrite Rules / Fusion
3. Cross Compilation
4. Profiling CPU
4. Memory Representation
6. Inlining


Binned amongst the **Web Development** user group, the most mentioned topics
are:

1. Profiling Memory
2. Profiling CPU
3. Laziness ( Strictness Annotations )

Binned amongst the **Compiler Design** user group, the most mentioned topics
are:

1. Cmm
2. STG
2. Memory Representation

Binned amongst the **Pure Mathematics or CS Theory** user group, the most
mentioned topics are:

1. Profiling Memory
2. Profiling CPU
3. Inlining

Binned amongst the **Data Analysis** user group, the most mentioned topics are:

1. Profiling Memory
3. Laziness ( Strictness Annotations )
3. Inlining 

The most mentioned write-ins were:

1. SIMD
2. Compiler Passes
3. Compiler Plugins

<p>
<img src="/images/Features_poll.png">
</p>

> Along the lines of performance profiling, I think GHC's execution model and heap representation are discussed less frequently than they deserve.

> Quality overviews on term rewriting and optimization steps on Haskell Core(System FC) in the GHC. I can tell it's out there, but information seems fragmented and a good quality article on the wiki would be very appreciated.

> Language interop, the C FFI is just the start of the story. How to play nice with the GC with foreign data? How to play nice with Haskell data from the other side?

#### Critical Comments

> In my work, arrows and categories are most useful in constructing lenses. I think lenses are actually a pretty simple idea but the most popular lens library is bloated and defines a multitude of esoteric infix operators.

> There doesn't seem to be any areas in any of the categories above which wouldn't benefit from more documentation. Almost all areas suffer from a lack of explained examples. The more I use almost any library, the more it seems to be lacking in good extensive documentation and examples.

> Conventions in web API client design and trade offs fr different choices. E.g. Typeclasses, free monads, etc. for example, suppose you want to make a web client gnostic of te underlying HTTP client, what's the best approach? Most people use typeclasses for this, but Haskell has many tools to tackle this problem.

> The "reactive" library seems to be very useful, but it is still very abstract. It would be nice to see more focus on this, providing more examples for how it can be used.

> I think the biggest use to myself and the community would be more articles like Gabriel Gonzalez has done that show how to use important Haskell constructs, like monoids or free monads, to structure help program design.

> There's some exciting developments in this area--see the Haste presentation by StrageLoop and others). And the efforts to bring React bindings to Haskell. All of the pieces exists in some form currently, but we have a ways to go before they mature.

#### Takeaway

This is of course an unscientific poll and please don't read too much into the
data. The goal was to generate a rough list of the topics that people are
interested in and feel need some more context.

On that note, if you are looking for topics for your next blog and want to
maximize the coverage of misunderstood topics and advance the state of Haskell
knowledge; consider one of the following subjects:

1. Types: **Impredicative Types**
1. Types: **Kind Polymorphism**
1. Types: **Singletons**
1. Language: **Profiling Memory**
1. Language: **Rewrite Rules / Fusion**
1. Language: **Cross Compilation**
1. Library: **repa**
1. Library: **uniplate**
1. Library: **mmorph**
1. Library: **free**
1. Library: **lens-family**
1. Pattern: **F-Algebras**
1. Pattern: **Cont Monad**
1. Pattern: **GHC.Generics**
