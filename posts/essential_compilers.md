---
title: A Haskell Implementation Reading List
date: December 14, 2014
---

### A Haskell Implementation Reading List

A reading list for those interested in the implementation internals of Haskell
compilers.  The GHC Reading list is [more
exhaustive](https://ghc.haskell.org/trac/ghc/wiki/ReadingList), these are simply
the papers I think give a good high-level overview of the topics.

**Typeclass Inference**

* [A Theory Of Qualified Types](http://ipaper.googlecode.com/git-history/969fbd798753dc0b10ea9efe5af7773ff10f728a/Mark-Jones/A-theory-of-qualified-types.pdf)
* [Type Inference For Parametric Type Classes](http://cpsc.yale.edu/sites/default/files/files/tr900.pdf)
* [Type Reconstruction For Type Classes](https://www4.in.tum.de/publ/papers/NipkowPrehofer_TRfT1995.pdf)
* [Constraints For Type Class Extensions](http://www.computerscience.nl/wiki/pub/Ehc/GvdGeest/geest07cnstr-tycls-ext.pdf)

**Typeclass Elaboration**

* [How to Make Ad-Hoc Polymorphism Less Ad-Hoc](http://202.3.77.10/users/karkare/courses/2010/cs653/Papers/ad-hoc-polymorphism.pdf)
* [Implementing Haskell Overloading](http://pdf.aminer.org/000/214/096/implementing_haskell_overloading.pdf)
* [Implementing Type Classes](http://pdf.aminer.org/000/542/781/implementing_type_classes.pdf)
* [Typeclasses In Haskell](http://ropas.snu.ac.kr/lib/dock/HaHaJoWa1996.pdf)

**Rank-N Types**

* [Practical Type Inference For Arbitrary Rank Types](http://repository.upenn.edu/cgi/viewcontent.cgi?article=1336&context=cis_papers)

**Core**

* [An External Representation for the GHC Core Language](https://downloads.haskell.org/~ghc/6.10.4/docs/html/ext-core/core.pdf)

**STG**

* [Making A Fast Curry](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/)
* [Implementing Lazy Functional Languages On Stock Hardware](http://research.microsoft.com/en-us/um/people/simonpj/papers/spineless-tagless-gmachine.ps.gz#26pub=34)
* [Implementing functional languages: a tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/student.djvu)

**Operational Details and Implementation**

* [Stack Traces In Haskell](http://arashrouhani.com/papers/master-thesis.pdf)

**Compilation**

* [Unboxed Types As First Class Values](http://www.haskell.org/ghc/docs/papers/unboxed-values.ps.gz)
* [Multi-paradigm Just-In-Time Compilation](http://www.cse.unsw.edu.au/~pls/thesis/dons-thesis.ps.gz)
* [Low Level Virtual Machine For Glasgow Haskell Compiler](https://www.cse.unsw.edu.au/~pls/thesis/davidt-thesis.pdf)
* [The Grin Project: A Highly Optimising Back End for Lazy Functional Languages](http://mirror.seize.it/papers/The%20GRIN%20Project.pdf)

**Transformations**

* [Secrets of the GHC Inliner](http://research.microsoft.com/en-us/um/people/simonpj/Papers/inlining/inline.pdf)

**Garbage Collection**

* [Parallel Generational-Copying Garbage Collection with a Block-Structured Heap](http://community.haskell.org/~simonmar/papers/parallel-gc.pdf)
* [Soft Real-time Garbage Collection for Dynamic Dispatch Languages](http://www.doc.ic.ac.uk/~amc4/Papers/thesis.pdf)

A copy of all of the PDF papers is available in this [Git repo](https://github.com/sdiehl/papers).
