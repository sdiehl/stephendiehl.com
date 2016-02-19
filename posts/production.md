---
title: The Joy and Agony of Haskell in Production
date: Feb 16, 2016
---

### The Joy and Agony of Haskell in Production

There have been [several](https://www.youtube.com/watch?v=AZQLkkDXy68) [good
talks](https://www.youtube.com/watch?v=mlTO510zO78) about using Haskell in
industry lately, and several people asked me to write about my personal
experiences.  Although I can't give specific details I will speak broadly about
some things I've learned and experienced.

**The myths are true.** Haskell code tends to be much more reliable, performant,
easy to refactor, and easier to incorporate with coworkers code without too much
thinking. It's also just enjoyable to write.

**The myths are sometimes trueisms.** Haskell code tends to be of high quality
by construction, but for several reasons that are only correlated; not causally
linked to the technical merits of Haskell. Just by virtue of language being
esoteric and having a relatively higher barrier to entry we'll end up working
with developers who would write above average code in *any* language. That said,
the language actively encourage thoughtful consideration of abstractions and a
"brutal" (as John Carmack noted) level of discipline that high quality code in
other languages would require, but are enforced in Haskell.

**Import libraries as qualified.** Typically this is just considered good
practice for business logic libraries. The only point of ambiguity I've seen is
amongst disagreement amongst developers on which core libraries are common
enough to import unqualified and how to handle symbols. This ranges the full
spectrum from fully qualifying ``(Control.Monad.>>=)`` to common things like
``(Data.Maybe.maybe)`` or just disambiguating names like ``(Map.lookup)``.

**Consider rolling an internal prelude**. As we've all learned the hard way, the
Prelude is not your friend. The consensus historically has favored the "Small
Prelude Assumption" which presupposes that tools get pushed out into third party
modules, even the core tools that are necessary to do anything (text,
bytestring, vector, etc).  This makes life easier for library authors at the
cost of some struggle for downstream users.

In practice any non-trivial business logic module can very easily have 100+
lines just of imports, and frankly it gets tiring. One common way of abstracting
this is by rolling a custom prelude using module reexports. Consider a minimal
use case like the following:

```haskell
module MegaCorpPrelude ( 
  module Exports,
) where

import Data.Int as Exports
import Data.Tuple as Exports
import Data.Maybe as Exports
import Data.String as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports

import Control.Monad.Trans.Except
  as Exports
  (ExceptT(ExceptT), Except, except, runExcept, runExceptT, 
   mapExcept, mapExceptT, withExcept, withExceptT)
```

This can be put into a cabal package which transitively pulls in the core
dependencies and then is used in our downstream module.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import MegaCorpPrelude
```

There are several custom preludes that are available on Hackage in the
[Prelude](https://hackage.haskell.org/packages/#cat:Prelude) category.

**Haskell has world class libraries**. There is an abundance of riches on
Hackage in libraries like quickcheck, mtl, pipes, conduit, tasty, attoparsec,
sbv and many more. Knowing [where to
start](http://www.haskellforall.com/2015/08/state-of-haskell-ecosystem-august-2015.html)
with the ecosystem can be a little tricky, and there are sometimes multiple
competing solutions. A conservative start to a library might consist of
something like the following ``build-depends`` in our cabal file:

```haskell
  build-depends:       
    base                 >= 4.6   && <4.9,
    deepseq              >= 1.3   && <1.5,
    hashable             >= 1.2.2 && <1.3,

    text                 >= 1.1   && <1.3,
    bytestring           >= 0.10  && <0.11,
    split                >= 0.2   && <0.3,

    unordered-containers >= 0.2   && <0.3,
    containers           >= 0.5   && <0.6,
    vector               >= 0.11  && <0.12

    mtl                  >= 2.2   && <3.0,
    transformers         >= 0.4   && <0.6,

    time                 >= 1.6   && <1.7,
    process              >= 1.1   && <1.3,
    directory            >= 1.2   && <1.3,
    optparse-applicative >= 0.10  && <0.13
```

**For many problem domains the libraries simply aren't written yet.** There are
many domains that Haskell is used in, and I've seen it used for tasks as diverse
as trading systems, spam filtering, to web services. Chances are there are a
plethora of libraries available some tasks. Yet it goes without saying that
Haskell is not Java or Python and there simply isn't the mindshare for certain
tasks. If we need to connect to Microsoft SQL Server or a SOAP service, we're
probably going to have more trouble. The primitives are probably there to do it,
but often there is no off-the-shelf solution.

Usually it boils down to: If you don't write that library, no one else will.

**There isn't a global concensus on how to write Haskell**. Being an abnormally
expressive languages means Haskell is written in such wildly different styles to
the point of being almost a different language in some cases. There are wildly
different views on how to structure large applications and not a whole lot is
written about best practices for doing so. Most of the schools of thought differ
about how far along the spectrum we should strive for correctness and what
power-to-weight ratio is appropriate for certain tasks.

It's hard to give universal advice about how to structure Haskell logic that
applies to *all problems*, and I'd be skeptical of anyone who did. For a certain
set of tasks that are command line utilities, processing pipelines, or web
services it's certainly possible to write applications that don't involve monad
transformers but there are certainly many other domain where the natural choice
is to roll a tree-like structure of monad transformer objects that encapsulate
common logic like Error and State.

If we look at the history of programming, there are  many portents of the future
of Haskell in the C++ community, another language where no two developers (that
I've met) agree on which subset of the language to use.

**Configuration** For configuration Bryan's ``configurator`` library is
invaluable. It species an external configuration file which can hold
credentials, connections and cluster topology information. A typical pattern is
to embed this in a ``ReaderT`` and then ``asks`` for any field necessary in
business logic.

```haskell
newtype ConfigM a = ConfigM (ReaderT ConnectInfo a)
  deriving (Monad, MonadReader ConnectInfo)

handleConfig :: FilePath -> IO ConnectInfo
handleConfig config_filename = do
    config <- Config.load [ Config.Required config_filename ]

    hostname <- Config.require config "database.hostname"
    username <- Config.require config "database.username"
    database <- Config.require config "database.database"
    password <- Config.require config "database.password"

    return $ ConnectInfo
     { connectHost     = hostname
     , connectUser     = username
     , connectDatabase = database
     , connectPort     = 5432
     , connectPassword = fromMaybe "" password
     }
```

The configuration file might look like the following:

```javascript
database
{
  hostname = "mydb.rds.amazonaws.com"
  database = "employees"
  username = "stephen"
  password = "hunter2"
}
```


**Haskell is full of distractions**. There are plenty of distractions that will
lead us down the wrong path or distract us from our core business. Working in
industry means that ultimately our job is create software that drives the
creation of revenue, not necessarily to advance the state of art. Although the
two are not necessarily mutually incompatible.

With this in mind, it's important to note there are plenty of vocal Haskellers
who work under a value system that is largely incompatible with industrial
practices. Much of which stems from hobbyists or academics who use Haskell as a
vehicle for their work. Not to diminish this category people or their work, yet
the metrics for success in this space are different enough that they tend to
view the programming space from a perspective that can be incommensurable to
industrial programmers.

The tensions of academic and industrial influences in Haskell is one of the
strong driving forces for progress; but it also leads to conflicts of interest
for many of us who write code to support ourselves. If one decides to engage
with the community, it's important to realize that many of the topics being
actively discussed are likely 3+ years out from being at the point where one
would want to bet the livelihood of a company on.

**Testing and building**. For development builds using cabal sandboxes it's
usually essential to be able to pull in internal libraries that are not on
Hackage. To do with cabal sandboxes this can be achieved with either a script to
provision 

```bash
$ git clone https://github.com/bscarlet/llvm-general
$ cd llvm-general
$ git checkout ca6489fdddde5c956a4032956e28099ff890a80b
$ cd ..
$ cabal sandbox add-source vendor/llvm-general-pure
```

With stack this can actually all be configured in the stack.yaml file.

```yaml
packages:
- location:
    git: https://github.com/bscarlet/llvm-general
    commit: ca6489fdddde5c956a4032956e28099ff890a80b
  subdirs:
    - llvm-general-pure/
```

Private TravisCI or Codeship are not worth the trouble of setting up if one ever
envisions the project spanning multiple repos. Getting their virtual machine
provisioned with the proper credentials to pull from multiple Github repos is
still a source of trouble. For build slaves and continuous integration I've used
BuildBot successfully to work with the usual cabal and stack toolchain.

For large multi-package builds, I can't speak highly enough of Neil Mitchell's
build system [shake](http://shakebuild.com/) which is itself written in Haskell.
The shake build uses Shakefiles which are monadic description of a graph of
dependencies to resolve and their artifacts. For a contrived example consider
running a Markdown file through Pandoc.

```haskell
import Development.Shake
import Development.Shake.FilePath

main = shakeArgs shakeOptions $ do
    want ["book.html"]
    "book.html" *> \out -> do
        need ["book.md"]
        system' "pandoc" ["book.md","-o","book.html"]
```

**Fast builds lead to faster turnaround.** If care isn't taken, projects can
quickly devolve to become unmanageably slow to compile, usually the problem is
avoidable with some care. Deriving instances of Read/Show/Data/Generic for
largely recursive ADTs can sometimes lead to quadratic memory behavior when the
nesting gets deep. The somewhat ugly hack to speed up compile time here is to
run ``ghc -ddump-deriv Module.hs`` and then manually insert the resulting code
instead of deriving it everytime. Not a great solution, but I've seen it
drastically improve compilation footprint and time. Also be tactical with uses
of ``INLINE`` and ``SPECIALIZE`` as inlining at many call sites has a
non-trivial cost. Avoid TemplateHaskell as it can cause ridiculously inflated
build times and enormous memory footprints in GHCi.

I'ts pretty common to use ghci and ghcid to during development stage. Your
mileage may also vary with ghc-mod support for Vim and Emacs which allows
in-editor type introspection.

Pulling from cabal to provision our test server can take minutes to hours
depending on the size of your dependency tree. Fortunately it's easy to set up a
[Hackage server mirror](https://github.com/haskell/hackage-server#mirroring)
that contains all of our internal dependencies that can be served quickly from
your local servers or an [S3
bucket](https://hackage.haskell.org/package/hackage-mirror). We can then simply
alter our ``~/.cabal/config`` to change the ``remote-repo`` to our custom
mirror. 

**Records** The record system is a continual source of pain. It's best to come
up with an internal convention for naming record accessors and use qualified
imports. It sucks for now, but there are some changes coming up the 8.0 release
that will make life easier.

When using Generics to derive ToJSON and FromJSON instances there is
``fieldLabelModifier`` field that can be used to modify the derived field so the
serialize does not have to match the Haskell record accessors. For example we'll
``drop`` the first three characters:

```haskell
data Login = Login
  { _lgusername :: Text
  , _lgpassword :: Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Login where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 3 }
```

This will serialize out to.

```javascript
{
  "username": "stephen"
  "password": "hunter2"
}
```

**Performance and Monitoring** A common performance problem is that of many
small updates updates to records with large numbers of fields. Records of
hundreds of fields are somewhat pathological but in practice they show up in a
lot of business logic that needs to interact with large database rows.  Too much
of this can very noticeable impact on GC pressure by doing allocations on each
update. If you notice runaway memory performance, one of the first places to
look (after the usual suspects) is look for overgrown records and possibly
inside of StateT with use of lots of modify`` in sequence.

The other very common library for live performance monitoring is ``ekg`` which
simply forks off a thread that manages the state of the GHC runtime internals
and can serve this data to other logging services via HTTP + JSON or via a web
server. For example:

```haskell
{-# Language OverloadedStrings #-}

import Control.Monad
import System.Remote.Monitoring

main :: IO ()
main = do
  ekg <- forkServer "localhost" 8000
  putStrLn "Started server on http://localhost:8000"
  forever $ getLine >>= putStrLn
```

ekg has several large dependencies so sometimes its desirable to optionally
enable it with a cabal configuration flag so that it's not included unless we
want a development build. We just qualify our build-depends to include it in the
dependencies if the flag is set via ``cabal configure -fekg``.

```yaml
flag ekg
  manual: True
  default: True
  description: Compile with ekg monitoring.

build-depends:
  if flag(ekg)
    build-depends:
      ekg >= 0.4 && < 0.5
```

**Strings** The strings types are mature, but unwieldy to work with in practice.
Make peace with the fact that in literally every module you'll have boilerplate
just to do simple manipulation and IO. ``OverloadedStrings`` overcomes some of
the issues, but it's still annoying that you'll end up playing string
type-tetris a lot.

If you end up rolling a custom prelude it's worth just correcting ``putStrLn``
and ``print`` to what they should be in a just world:

```haskell
-- IO
putStr :: MonadIO m => Text -> m ()
putStr = liftIO . Data.Text.IO.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . Data.Text.IO.putStrLn

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print
```

A common pattern is to use a multiparamter typeclass to do string conversions
between all the common (Data.Text.Text, Data.Text.Lazy, Data.ByteString.UTF8
Data.ByteString.Lazy.UTF8, [Char]) types. You'll end up eating at least one
typeclass dictionary lookup per call to ``s`` but this is fairly benign in most
cases.

```haskell
class StringConvert a b where
  s :: a -> b

instance (ToString a, FromString b) => StringConvert a b where
  s = fromString . toString

instance FromString UTF8.ByteString where
    fromString = UTF8.fromString

instance FromString LUTF8.ByteString where
    fromString = LUTF8.fromString

instance ToString UTF8.ByteString where
    toString = UTF8.toString

instance ToString LUTF8.ByteString where
    toString = LUTF8.toString
```

There are libraries on hackage (
[string-convert](https://hackage.haskell.org/package/string-convert-2.0.1),
[string-conv](https://hackage.haskell.org/package/string-conv-0.1) ) that
implement this pattern.

**Documentation is abysmal**. Open source Haskell libraries are typically
released with below average or non-existent documentation. The reasons for this
are complicated confluence of technical and social phenomena that can't really
be traced back to one root cause. *Basically, it's complicated.* What this means
for industrial use is to always budget extra hours of lost productivity needed
to reverse engineering libraries from their test suites just to get an minimal
example running. It's not great, but that's the state of things.

**You own the transitive dependency tree**. With such a small ecosystem,
anything we pull in you have to be able to maintain and support should the
upstream source dry up or wither on the vine. The cold truth is if there's no
community-provided documentation for the library and you depend on it for your
product, you've just added technical debt to your company. The person you have
to hand the code off to will have to read through your code and all it's
transitive dependencies, and while the undocumented upstream libs might make
sense they may utterly confound your predecessor.

If you're depending on your Haskell code being stable and supportable it's worth
being conservative in what dependencies you pull into your tree.

**Avoid TemplateHaskell**. Enough said, it's a eternal source of pain and sorrow
that I never want to see anywhere near code that I had to maintain
professionally. The best quote about this is found in this [StackOverlow
thread](https://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell):

> Think about it. Half the appeal of Haskell is that its high-level design allows
> you to avoid huge amounts of useless boilerplate code that you have to write in
> other languages. If you need compile-time code generation, you're basically
> saying that either your language or your application design has failed you.

If you need to crank out that much boilerplate just to get something done, go
back and rethink. If a upstream library forces you to use it, don't depend on
that library. There is almost always a way to accomplish the task without
falling back on TH.

**Don't be afraid to train people.** Contrary to a lot of popular myths, with
the right direction people can indeed pick up Haskell quite quickly. There are
great developers outside the community who given a little bit of insight into
Haskell will turn into great Haskellers. People who have a little experience
with Scheme, Clojure, Scala, Ocaml can pretty quickly learn the language.

I was fortunate enough to train an a very talented intern named Dan who came in
not knowing any Haskell (was primarily a Java developer) and in two weeks had
picked up the language and was amazingly productive. Learning on your own is
much more time consuming than having a Haskell friend sitting next to you. It's
a time investment, but it can pay off exponentially with the right person and
with Dan it most certainly did.

**Network with other industrial users.** There is no shortage of hobbyist
Haskell programmers to consult with about problems. Though by my estimates in
the United States there are probably only around 30-40 people working on Haskell
fulltime and a good deal more working part-time or anticipating using it. It's
worth networking with other industrial users to share best practices.
