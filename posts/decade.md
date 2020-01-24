---
title: Haskell For a New Decade
date: January 1, 2020
---

### Haskell Problems For a New Decade

It has been a decade since I started writing Haskell, and I look back on all the
projects that I cut my teeth on back in the early part of this decade and
realise how far the language and tooling have come. Back then Haskell was really
barely usable outside of the few people who would "go dark" for months to
learn it or those lucky enough to study under researchers working on it. These
days it still remains quite alien and different to most mainstream languages. However,
it's now _much_ more accessible and exciting to work with.

We programming communities always like to believe our best days are ahead
of us and our worst days behind us. But it's the right now that's
the issue and always has been. The problems we work on in the present are those
that _shape_ the future, and often the _choice of problems_ is what matters more than anything else.

At the turn of the century, the mathematician David Hilbert laid out 23 problems
for mathematicians to solve in the 19th century. These were the Big Hairy
Audacious Goals (BHAG) for the program of mathematics at the time, problems
that drove forward progress and were exciting, adventurous areas to work in.
Haskell has always been at the frontier of what is possible in computer science, and it also sustains a devoted community that regularly drags the future into the
present. This can't be done without the people who dare to dream big and build
toward ambitious projects.

Here I am proposing a set of ambitious problems for the next decade:

#### Algebraic Effect Systems

The last few years have seen a lot of emerging work with new and practical
effect systems. These are alternative approaches to the mtl style of modeling
effects that has dominated Haskell development for the last decade. These effect-system libraries may help to achieve a boilerplate-free nirvana of tracking algebraic effects at much more granular levels.

In the usual Haskell tradition,
there are several models exploring different points in the design space. In their current state, these projects introduce quite a bit of overhead, and some of them even require a plethora of GHC plugins to optimise away certain
abstractions or to fix type inference problems. Still, it's likely that one of these projects will see critical adoption in this decade:

1. [fused-effects](https://github.com/fused-effects/fused-effects)
2. [polysemy](https://github.com/polysemy-research/polysemy)
3. [eff](https://github.com/hasura/eff)

I predict that by 2030 one of these models will emerge as the next successor to mtl and that
we will have a standard `Control.Effect` module inside of the Prelude with
language-integrated support in GHC. There will likely be a few more years of
experimentation before this occurs, but this will become the standard way of modeling
effects in Haskell programs in the next decade.

#### Practical Dependent Types

Today you can achieve a measure of dependent types in Haskell by enabling enough
language extensions and using frameworks like [`singletons`](https://hackage.haskell.org/package/singletons). This experience is not a
pleasant one, and there have long been discussions about whether there exists a
sensible migration path to full dependent types that won't kill the golden
goose of GHC in the process.

Personally, I think GHC, with its increasingly rich System-F, represents a local
maxima (not a global maxima) in the functional language design space but it is one that
produces a massive amount of economic value. Tens of millions of dollars depend
on GHC maintaining compatibility with its existing ecosystem, and I'm a bit
frightened that massive changes to the core language would become a bridge too far
to cross for industrial users.

On the opposite side, companies like Galois and Github have [pushed the
language](http://www.davidchristiansen.dk/pubs/dependent-haskell-experience-report.pdf)
to the limits of what is possible. However, this is still not the predominant paradigm of
writing Haskell and it comes with some rather serious tradeoffs for the level of
power it provides.

Achieving full dependent types is probably the biggest and hairiest problem in Haskell and one that would require a massive amount of time, funding, and interdisciplinary collaboration to
achieve. There is perhaps a seamless path to full dependent types, so this may
be the decade in which Haskell puts the problem to rest in favour of a new decade of
dependent type supremacy.

#### Lower barriers to GHC Development

GHC Haskell is a miracle. It is an amazing compiler that has moved our
discipline forward decades. That said, GHC is itself not the most accessible
codebase in the world due to the inherent complexity of the engineering involved.

Haskell is also not immune to the open source sustainability problems that widely-used projects suffer from. As such, GHC development is extremely resource starved and this fact
poses an existential threat to the continued existence of the language. Indeed, a
certain percentage of the _users_ of GHC will have to become to
_maintainers_ of GHC if the ecosystem is going to continue.

As a funny historical quirk, back in 2011 there was an interview with Ryan Dahl,
the creator of NodeJS,
who [mentioned](https://www.americaninno.com/boston/node-js-interview-4-questions-with-creator-ryan-dahl/)
that the perceived difficulty in writing a new IO manager for GHC was a factor in the
development of a new language called NodeJS. When asked why he chose Javascript, for the project, he replied:

> Originally I didn’t. I had several failed private projects doing the same on
> C, Lua, and Haskell. Haskell is pretty ideal but I’m not smart enough to
> hack the GHC.

Simon always says to think of GHC as "your compiler" and while it might be a
scary codebase, if you are reading this you probably already are smart
enough to hack on GHC (although there is a kind of weird twist of fate that
Haskell may itself have spawned NodeJS).

All things considered, since 2011 the barriers to
entry have gone monotonically down as more people have become familiar with the
codebase. There are now lovely new Nix environments for doing rapid GHC
development, [beautiful
documentation](https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf),
adaptors for working with the GHC API across versions, and the new Hadrian build
system.

If you look at the [commit
logs](https://gitlab.haskell.org/ghc/ghc/-/graphs/master) to GHC itself you'll
see a lot of recent development dominated by about 10 or so supercommiters and a
variety of smaller contributors. As a conservative goal, if there were 2 more
regular contributors to GHC every year, by 2030 there would be 20 more
contributors, and the ecosystem would have a significantly lower bus-factor as a result.

#### Faster Compile Times

The singularly biggest issue most industrial users of Haskell face are the long
build times with the enormously large memory footprints. GHC itself is not a
lightweight compiler and it performs an enormous amount of program transformation that
comes at a cost. One might argue that the compile-time costs are simply a
tradeoff that one makes in exchange for all the bugs that will never be
introduced, but most of us find this an unsatisfying compromise.

GHC spends the majority of its time in the simplifier. Thus, all the big wins in GHC
compile-times are to be had in optimising the simplifier. This is not
necessarily low-hanging fruit but a lot of this is just a matter of engineering
time devoted toward profiling and bringing the costs down. The only reason this
isn't moving forward is almost certainly due to a lack of volunteers to do the work.

Imagine in 2030 compiling your average modern Haskell
module 5x faster and using half the memory as GHC 8.10! Now that's a big hairy goal worth pursuing.

#### Editor Tooling

The [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine) (HIE)
project has been developing slowly for the better part of the last five years.
The project has gotten quite stable and is a fully-featured implementation of
the Language Server Protocol which can integrate with Vim, VSCode, Emacs etc.

GHC itself has developed a new approach to generating editor tagging called [HIE
Files](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html) which promise to
give much better support for symbol lookup in IDEs. There is also rough tooling
supporting the new Language Server Index Format which will give GHC much better
integration with Github and Language Server ecosystem. The new
[`hie-bios`](https://github.com/mpickering/hie-bios) library really helps in
setting up GHC sessions and configuring GHC's use as a library for syntactic
analysis tools.

The project itself is still a bit heavy to install, taking about 50 minutes to
compile and it uses quite a bit of memory running in the background. Tab completion
and refactoring tools are "best effort" in many cases but often become quite
sluggish on large codebases. That aside, these are largely optimisation and
engineering problems that are generally tractable given enough time and engineering
effort.

By 2030, Haskell could have world-class editor integration with extremely
optimised tab completion, in-editor type search, hole-filling integration, and
automatic refactoring tools. Of course, there are also truly magical, type-based editor
tools that have yet to be invented. 🦄

#### Compiler Modularity

Almost every successful language ultimately ends up spinning off a few research
dialects that explore different points in the design space. Python for instance
has mainline CPython, but also Jython, PyPy, Unladen Swallow, etc. In the last
decade there was the [Haskell Suite](https://github.com/haskell-suite) project
which was a particularly brilliant idea around building a full end-to-end
Haskell compiler as a set of independent libraries. The pace of GHC development
ultimately makes this a very labor intensive project, but the idea is sound and
the benefits to even having a minimal "unfolded compiler" would likely be
enormous. Instead of requiring multiple grad students to prototype a compiler, academic researchers could simply use an existing compiler _component_ for the parts that aren't relevant to their research.

Similarly, the [Grin](https://github.com/grin-compiler/ghc-grin) project has invested a heroic
amount of effort in attempting to build a new retargetable backend for a variety
of functional languages such as Idris, GHC, and Agda. This kind of model should
inspire others for different segments of the compiler. The devil is in the
details for this project, of course, but if we had this kind of modular framework,
the entire functional language space would benefit from an increase in the pace
of research.

#### GraalVM Target

Anyone who has tried to get Haskell deployed inside an enterprise environment
will quickly come up against a common roadblock: "If it doesn't run on the JVM,
it doesn't run here. Period." It's a bitter pill to swallow for some of us but
it is a fundamental reality of industry.

Java-only environments are largely the norm in enormous swathes of the
industry. These aren't the startups or hot tech companies, but the bulk of the
large, boring companies that run everything in the world. They are generally risk averse and anything that doesn't run on the JVM is banned. Eventually, there may be a sea change in IT attitudes, but I doubt it will happen this decade.

The good news is that the JVM ecosystem isn't nearly as bad as it used to be: there are several emerging compiler targets in GraalVM, the Truffle Framework, and
Sulong that have drastically reduced the barriers to targeting the JVM. A heroic-level task for a very ambitious Haskeller would be to create a JVM-based runtime for GHC Haskell to compile to. There have been a few attempts to do this over the years with [Eta Lang](https://eta-lang.org/) but 2030 could be the decade
when this finally becomes possible.

#### Build Tools

Cabal has gone through several developments over the last decade. In its latest iteration with its `new-`style commands, `cabal` has reached a reasonable level of stability and it now works quite nicely out of the box.

Unfortunately, Cabal gets a lot of undue grief. Since Haskell happens to link all
packages during build time, when two packages conflict with each other the last
command entered at the terminal was undoubtedly a `cabal` command. As a result, rather than blaming the
libraries themselves, people tend to direct their anger at Cabal since it appears
to be the nexus of all failures. This has lead to a sort of "packaging nihilism"
as of late. The sensible answer isn't to burn everything to the ground; instead, it's
slow, incremental progress toward smoothing away the rough edges.

These days both Stack and Cabal can adequately build small projects and manage
dependencies in both of their respective models. However, both tend to break down when
trying to manage a very large, multi-project monorepo as it doesn't use any
sandboxing for reproducible builds or incremental caching.

Moreover, the barrier to entry to using both of these systems is strictly higher than in
other languages, which begs the question: is there some higher
level project setup and management tool which can abstract away the complexity?
The precise details of this are unclear to me but it is certainly a big hairy
problem, and probably the most thankless one to work on.

#### WebAssembly

Haskell needs to become a first class citizen in the WebAssembly ecosystem. The
WebAsembly ecosytem is still lacking a key motivating use case in the
browser, but in spite of that it is emerging as a standardised target for a variety of platforms
outside of the web. I can't quite predict whether WebAssembly will become
embroiled in web committee hell and wither on the vine or continue to accelerate in uptake, but I think we will have
more clarity on this in the coming years. There are some early efforts at
bolting WebAssembly onto [GHC](https://github.com/tweag/asterius) which show promise.
There are also nice toolchains for building and manipulating [Assembly
AST](https://hackage.haskell.org/package/wasm) and exchanging between textual
and bitcode formats.

#### Deep Learning Frameworks

The last decade also saw the advancement of deep learning frameworks which
allow users to construct dataflow graphs that describe the [different
topologies](https://towardsdatascience.com/the-mostly-complete-chart-of-neural-networks-explained-3fb6f2367464?)
of matrix operations involved in building neural networks. These kind of
embedded graph constructions are quite natural to build in Haskell, and the only
reason there isn't a standard equivalent in Haskell is likely simply a) time and
b) the lack of a standard around unboxed matrix datatypes. For one example, there are some quite
advanced bindings to the Torch C++ librires in the
[HaskTorch](https://github.com/hasktorch/hasktorch) project.

I used to work quite heavily on data science in Python, and I'm convinced the
entire PyData ecosystem is actually a miracle made of magic fairy dust that has been
extracted out of the crushed remains of academic careers. That said, the Python
ecosystem lacks a robust framework for building embedded domains for non-Python
semantics. Many of the large tech companies are investing in alternative
languages such as [Swift](https://www.tensorflow.org/swift) and [Julia](https://julialang.org/) in order to build the next iterations of these libraries because of the hard limitations of CPython.

Here, there exists a massive opportunity for someone to start working on this family of
[differentiable
computing](https://github.com/apple/swift/blob/master/docs/DifferentiableProgramming.md)
libraries in Haskell. It is a huge investment in time, but also has an outsize economic
upside if done well.

#### Fix Records

Haskell is really the only popular language where record syntax diverges from
syntactic norms of dot-notation. The `RecordDotSyntax` or some revised version of that proposal
ideally should be folded into GHC this decade. Admittedly, the GHC syntax zoo is getting
trickier to herd as it grows in complexity, but this is one of those changes
where the reward vastly outweighs the potential breakage. We've navigated worse
breaking changes in the past and this one change would really set the language
on a different course.

Records are probably the last legacy issue to overcome, and I have faith this will
be the decade we'll finally crack this one.

#### Refinements and Invariants

Awhile back GHC added support for adding arbitrary annotations to source code.
This quickly gave rise to an ecosystem of tools like [LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell-blog/) which can use
GHC source code enriched with invariants and preconditions that can be fed to
external solvers. This drastically expands the proving power of the type system
and lets us make even more invalid states inaccessible, including ones that
require complex proof obligations beyond the scope of the type system.

This said, programming with SMT solvers is still not widely practiced in the ecosystem, nor is writing additional invariants and
specifications. There is definitely a
wide opportunity to use this powerful tooling and start integrating it into
industrial codebases to provide even more type-safe APIs and high-assurance
code.

On top of this, new ideas like [Ghosts of Departed
Proofs](https://hackage.haskell.org/package/gdp) present
new ways of encoding invariants at the type-level and in building reusable frameworks for
building new libraries. The ecosystem has largely not incorporated such ideas
and the big hairy goal of the next decade will be standardisation of these
practises of enriched type annotations and proof systems.

#### Small Reference Compiler

Most undergraduates take a compiler course in which they implement C, Java or
Scheme. I have yet to see a course at any university, however, in which Haskell is used
as the project language. It is within possibility that an undergraduate
course could build up a small Haskell compiler during the course of a
semester. If the compiler had a [reasonable
set](https://github.com/Jaak/TinyOutsideIn) of features such as algebraic data
types, ad-hoc polymorphism, and rank-n types this would prove to be quite an interesting
project. The leap from teaching
undergraduates to code Scheme to Modern Haskell is a bit of a leap but it's 2020
now; we're living in the future and we need to teach the future to our next
generation of compiler engineers. The closest project I've seen to this is a minimal Haskell dialect called [duet](https://github.com/chrisdone/duet).

This is a big hairy project that involves creating a small compiler and a
syllabus and then trying it out on some students.

#### Type-driven Web Development

The Servant ecosystem has come quite far in recent years. What started as
an experimental project in building well-typed REST APIs has become a widely
used framework for building industrial codebases. Granted, the limited breadth of
Haskell's ecosystem means that it will never be on parity with other widely used
web languages. Nevertheless, a framework like Servant offers a unique set of upsides that
is particularly appealing for codebases that are already written in Haskell and
need web API exposure.

While the ecosystem is maturing there are definitely some large holes to fill to
achieve parity with other languages and frameworks. Indeed, Servant sits at the foundation of many Haskell web applications, but it requires a variety of additional layers and
custom code to build a traditional business application with. Those using Servant in their business applications should consider that contributing these
additional layers back in the form of resuable components would have a massive impact on the viability of web projects outside of their own companies. These contributions could move the needle on Haskell uptake in web applications.

#### Project-Driven Books

The corpus of advanced Haskell knowledge is often mostly gathered through
discussions and vast amounts of time reading through code. In the last few years
we saw a few authors step up and write Haskell books. In particular [Thinking
With Types](https://leanpub.com/thinking-with-types) stands out as a rare
example of a text which tackles more advanced topics instead of introductory
material. Compared with other language ecosystems, there has always been a wide gap in both material covering
intermediate Haskell and project-driven texts. Even so, a vast hunger remains present in the community
for such material, and filling this gap would advance efforts to onboard new
employees as well as help novices move to more advanced levels of Haskell coding.

The economics of writing a book through a technical publisher like O'Reilly, however,
are a bit rubbish and tend to favour the publisher over the author. If you set
out with an advance from a publisher, rest assured it is not going to be a path
to many financial returns for your investment in time. Writing always takes longer
than you think! With that said, the investment from a few coauthors could really move the
language forward on a staggering level: the broadly-expressed desire for such materials means a large audience is waiting.

#### Computational Integrity Proofs

In the last few years one of the silent advances in computer science has been in
a niche area of cryptography known as verifiable computing. I gave an overview
of this topic at ZuriHac back in 2017 and have been working steadily on this
research for several years now. Long story short, there is a brilliant new set of
ideas that allow the creation and execution of arbitrary computations in a data-oblivious way (so called zero knowledge proofs or zkSNARKs) that, combined with a
bit of abstract algebra, give a way for pairs of counterparties (called provers
and verifiers) to produce sound proofs of correct execution with minimal
assumptions of trust. For a long time the constructions involved were too
computationally expensive to be practical but if you draw a line through the
state of the art proof systems for the last 10 years the line has been
monotonically decreasing in the time cost for proof construction and
verification. If trends continue, by the middle of this decade this should give
rise to a very powerful new framework for sharing computation across the
internet.

This is still early work but I have made available most of the work involved in this
[research](https://github.com/adjoint-io/arithmetic-circuits) for
others to [build on](https://github.com/adjoint-io/zkp). The many recent
developments in Haskell cryptography libraries have positioned the ecosystem
optimally for building these kind of frameworks over the next decade.

#### Relocatable Code

There have been many conversations about the dream of being able to serialise the
Haskell AST over a network and then to evaluate code dynamically on remote servers.
There have also been many [stalwart
attempts](https://hackage.haskell.org/package/distributed-closure) to build the
primitives required but standing in the way are some legitimately hard engineering problems
that require environment-handling and potential changes to the language itself.

Languages which can serialise to bytecode often times have a distinct advantage
in this domain where the problem becomes much simpler. The nature of how GHC
Haskell is compiled limits this approach and much more clever work will need to
be done. In 2030, if we had a reliable way to package up a Haskell function along
with all its transitive dependencies and ship this executable closure over the
wire this would allow all sorts of amazing applications.

#### Employment

Haskell is never really going to be a mainstream programming language. The
larger programming ecosystem is entirely dominated by strong network forces that make it impossible for small community-driven languages to thrive, except
in a few niche areas. This isn't cause for despair; instead, we should really
just focus on expanding the "carrying capacity" of the ecosystem we have and
focus on making Haskell excel in the areas it is well-suited for.

Progress in the open source Haskell ecosystem has always been dominated by
engineers working in small startups to mid-size enterprises more so than large
organisations. I strongly encourage other Haskellers to start companies and
teams where you have the autonomy to use the tools you want to serve the market
opportunities you want. The experience is enormously stressful and will shave
years off your life but it is a rewarding one.

The big hairy goal here is for some ambitious folks to start new ventures. Modern dev teams are always going to be polyglot. The rub is always to ensure that Haskell is used industrially in places where it is well-suited, and not used in places where it is not. When used poorly it can cause massive technical debt, but when tactically applied, Haskell can give your architecture a huge market advantage.

#### Documentation

Haskell documentation has gotten much better over the last few years. This is not to say
that it's in a great situation but there's now enough to muddle through. If you look
at the [top 100](https://hackage.haskell.org/packages/top) packages on Hackage,
around a third of them have proper documentation showing the simple use cases
for the library. This is markedly improved from years ago when any semblance of
documentation was extraordinarily rare. The [hyperlinked source
code](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html)
on Hackage has also made traversing through the ecosystem quite friendly.

By 2030 we can hope that about half of the top 100 packages will have some
measure of documentation. Additionally, the continued proliferation of static
types in other ecosystems will acclimate new users to the type-driven model of
documentation. The Github ecosystem will likely have Haskell language
information built and "go to definition" across packages will decrease the
barriers to entry to exploring new libraries. All of this will further aid adoption and newcomers to the language.

### In Conclusion

Some of the ideas outlined above, such as the need for more contributors to GHC, are vital for the viability of Haskell in the long-term. Overall, we should start addressing these big hairy goals in order to achieve not only a healthy Haskell ecosystem by 2030, but one that continues the tradition of innovation and technical leadership that the Haskell community has shown in its history.
