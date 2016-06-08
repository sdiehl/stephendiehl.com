---
title: Building a Better Custom Haskell Prelude
date: April 7, 2016
---

### Building a Better Custom Haskell Prelude

The Haskell Prelude is the default import into all Haskell modules, it provides
an endless number of ways to shoot ourselves in the foot and historical cruft
that can't be removed. While it is difficult to fix upstream, we can however
remove the Prelude entirely on a project-level and replace it with a more
sensible set of defaults using the ``-XNoImplicitPrelude`` language extension.

There are two philosophies on building new Preludes:

* *Big Vehicle Prelude* - Fix most of the deficiencies in the Prelude by
  introducing new abstractions that replace large portions of the basic types
  and class, and generally redefine the way we write Haskell. See
  [numeric-prelude](https://hackage.haskell.org/package/numeric-prelude).
* *Small Vehicle Prelude* - Fix the broken parts of the Prelude by building on
  existing types and classes and masking broken bits. See
  [basic-prelude](https://hackage.haskell.org/package/numeric-prelude).

I don't prescribe to the large vehicle approach, a lot of the default Prelude is
not ideal but good enough to get the job done. And interoperability with the
rest of the ecosystem, which typically uses default Base and Prelude, is
incredibly important. 

In [previous posts](http://www.stephendiehl.com/posts/production.html)  I've
written about how rolling our own small-vehicle Prelude is a good idea for large
teams working in industry but I left it somewhat ambiguous about what to
include. Obviously everyone's company is different so I thought I'd expand on
what I consider a sensible set of defaults that one could use to then build a
custom Prelude.  We'll call this a *Protolude*.

<p style="text-align:center">
**[Git Project](https://github.com/sdiehl/protolude)**
</p>


#### Module Structure

The basic module structure is consists of a main module reexport which we'll
dump everything in called ``X``. And various miscellaneous functions which
we'll reexport from the root module. Off of this we'll have several utilities
modules which have domain specific functions.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude (
  module X,
  identity,
  bool,
  (&),
  uncons,
  applyN,
  print,

  LText,
  LByteString,
) where

import qualified Prelude as P

import qualified List as X
import qualified Show as X
import qualified Bool as X
import qualified Debug as X
import qualified Monad as X
import qualified Applicative as X
```

#### Base Types

The Basic types (``Int``, ``Integer``, ``Float``, ``Char``) we reexport wholesale
along with their associated functions.

```haskell
-- Base types
import Data.Int as X
import Data.Bits as X
import Data.Word as X
import Data.Bool as X hiding (bool)
import Data.Char as X (Char)
import Data.Maybe as X hiding (fromJust)
import Data.Either as X
import Data.Complex as X

import Data.Function as X (
    id
  , const
  , (.)
  , flip
  , fix
  , on
  )
```

We define the flipped application operator for GHC < 7.10 which does not provide
this be default.

```haskell
(&) :: a -> (a -> b) -> b
x & f = f x
```

We rename the ``id`` function to identity since it's far more likely we'll have
variable names called ``id`` over the identity function. This is a contentious
point and your mileage may vary with this change.

```haskell
identity :: a -> a
identity x = x
```

The bool function (the combinator analogue of ``maybe`` and ``either``) is
provided by GHC 7.10, but we reexport it for earlier versions.

```haskell
bool :: a -> a -> Bool -> a
bool f t b = if b then t else f
```

The ``uncons`` function is not provided by default, but it unpacks a list into a
Maybe tuple containing the head in the first tuple parameter if it exists.

```haskell
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)
```

The ``applyN`` function takes a function and a count and applies it ``n`` number
of times.

```haskell
applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) id (X.replicate n f)
```

And then we reexport various boolean combinators for working with branching
monadic logic.

```haskell
whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f
```

#### Safe

[Safe](https://hackage.haskell.org/package/safe) provides Maybe versions of many
of the various partial functions (``head``, ``tail``) that are shipped by
default. Wrapping it up in a Maybe is widely considered the right approach and
if Haskell were designed today, they would not be present.

```haskell
-- Maybe'ized version of partial functions
import Safe as X (
  headMay,
  initMay,
  tailMay
  )
```

#### Debugging

Various debugging and trap commands are still provided since they are useful for
partial code and various fatal program logic. However now they have a warning
emitted by the compiler if they are left in place, with position information
about where they are used. This can be toggled between production code and
debugging code.

```haskell
{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = P.undefined

{-# WARNING error "'error' remains in code" #-}
error :: P.String -> a
error = P.error

{-# WARNING trace "'trace' remains in code" #-}
trace :: P.String -> a -> a
trace = T.trace

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> a
traceShow a = T.trace (P.show a) a

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, P.Monad m) => a -> m ()
traceShowM a = T.traceM (P.show a)

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: P.Monad m => P.String -> m ()
traceM = T.traceM

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: P.String -> P.IO ()
traceIO = T.traceIO

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = P.error "Not implemented"
```

#### Either

Various either combinators to convert between Maybe and Either probably should
be provided but are not, there are four various combinations of these. Handling
a maybe which happens to have a monoidal instance for mempty in Left is also
fairly common task.

```haskell
leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToEither :: Monoid b => (a -> b) -> Maybe a -> b
maybeToEither = maybe mempty
```

#### List

The various list functions are exported wholesale excpet for the ones which are
generalized by Foldable and Traversable. head is reexported as a total function
written in terms of foldr. The ``ordNub`` function is accidentally quadratic in
the Prelude is replaced by a logarithmic variant using a hashmap underneath.

```haskell
head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> return x) Nothing

sortOn :: (Ord o) => (a -> o) -> [a] -> [a]
sortOn = sortBy . comparing

-- O(n * log n)
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs
```

#### Monad

The core moand class, functions, and combinators are reexported wholesale.
Variants like ``mapM`` are provided by ``Data.Traversable`` instead of
``Control.Monad``.

```haskell
module Monad (
    Monad(..)
  , MonadPlus(..)

  , (=<<)
  , (>=>)
  , (<=<)
  , forever

  , join
  , mfilter
  , filterM
  , mapAndUnzipM
  , zipWithM
  , zipWithM_
  , foldM
  , foldM_
  , replicateM
  , replicateM_
  , concatMapM

  , guard
  , when
  , unless

  , liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  , liftM'
  , liftM2'
  , ap

  , (<$!>)
  ) where

import Prelude (concat, seq)
import Control.Monad

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
```

Strict versions of ``liftM`` are also provided by default since this is a common
source of unexpected laziness.

```haskell
liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' = (<$!>)
{-# INLINE liftM' #-}

liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  x <- a
  y <- b
  let z = f x y
  z `seq` return z
{-# INLINE liftM2' #-}
```

#### Applicative

The applicative module core types are reexported wholesale. Unlike in 7.8 we
have the Applicative class in scope by default, as it should be in this day and
age.

```haskell
-- Applicatives
import Control.Applicative as X (
    Applicative(..)
  , Alternative(..)
  , Const(..)
  , ZipList(..)
  , (<**>)
  , liftA
  , liftA2
  , liftA3
  , optional
  )
```

#### Typeclasses

The core GHC typeclasses for ordering and numeric tower are reexported
wholesale. We also bring Traversable and Foldable into scope masking a few of
the partial functions which generally should be avoided. Semiring is also common
enough these days that it should be in scope implicitly.

```haskell
-- Base typeclasses
import Data.Eq as X
import Data.Ord as X
import Data.Monoid as X
import Data.Traversable as X
import Data.Foldable as X hiding (
    foldr1
  , foldl1
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  )
import Data.Semiring as X
import Data.Functor.Identity as X
import Data.Functor as X (
    Functor(..)
  , ($>)
  , (<$>)
  , void
  )
```

#### Deepseq

Deepseq is usually an important typeclass to derive for various usecases, and so
we bring in the ``deepseq`` library and it's various functions.

```haskell
-- Deepseq
import Control.DeepSeq as X (
    NFData(..)
  , ($!!)
  , deepseq
  , force
  )
```

#### Data Structures

The core data structures provided by GHC are reexported, but with List we make
sure to mask the partial functions, and operators are provided in more general
interfaces.

```haskell
-- Data structures
import Data.Tuple as X
import Data.List as X (
    splitAt
  , break
  , intercalate
  , isPrefixOf
  , drop
  , filter
  , reverse
  , replicate
  )
```

The containers library all provide functions which overlap with each other
(lookup, insert, etc) so we just export the types for these structures so we can
use them in signatures. On a module level these would then be typically imported
using a qualified import like ``import qualified Data.Map as Map``.

```haskell
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)
```

#### Monad Transformers

The basic ``mtl`` transformer stack are usually ubiqitious in modern Haskell,
specifically StateT, ReaderT and ExceptT. We don't export all functions in these
modules but just enough that most uses cases of the common transformers are
brought into scope.

```haskell
-- Monad transformers
import Control.Monad.State as X (
    MonadState,
    State,
    StateT,
    put,
    get,
    gets,
    modify,
    withState,

    runStateT,
    execStateT,
    evalStateT,
  )

import Control.Monad.Reader as X (
    MonadReader,
    Reader,
    ReaderT,
    ask,
    asks,
    local,
    runReader,
    runReaderT,
  )

import Control.Monad.Except as X (
    MonadError,
    Except,
    ExceptT,
    throwError,
    catchError,
    runExcept,
    runExceptT,
  )

import Control.Monad.Trans as X (
    MonadIO,
    lift,
    liftIO,
  )
```

#### Wired-In Types

Several basic types (IO, Show) are necessary to do anything so we obviously
bring these in. The ``Exts`` provides the various pointer types for working with
FFI as well as the Constraint type which is somewhat common in the presence of
``-XConstraintKinds`` and more advanced type family techniques.

```haskell
-- Base GHC types
import GHC.IO as X (IO)
import GHC.Num as X
import GHC.Real as X
import GHC.Float as X
import GHC.Show as X
import GHC.Exts as X (
    Constraint
  , Ptr
  , FunPtr
  , the
  )
```

#### Generics

Generics are also ubiqitious in modern Haskell and the various typeclasses and
type synonyms should be provided so that we can ``-XDeriveGeneric`` as well as
implement default instances for Generic classes. 

```haskell
-- Generics
import GHC.Generics (
    Generic(..)
  , Rep
  , K1(..)
  , M1(..)
  , U1(..)
  , V1
  , D1
  , C1
  , S1
  , (:+:)
  , (:*:)
  , NoSelector
  , Rec0
  , Par0
  , Constructor(..)
  , Selector(..)
  , Arity(..)
  , Fixity(..)
  )
```

#### String Types

Strings in Haskell are a giant fractal of suffering. In a just world, we would
all just use Text most of the time, and ByteString when we needed to deal with
network but we don't live in that world an large portions of Hasell ecosystem
still use the wildly-inefficient linked-list of Char.

Controversially we just reexport the Lazy text type as a type synonym ``LText``
and provide the ``string-conv`` library function ``toS`` which automatically
converts between any two string types using  a multiparam typeclass. String
heavy programs might also consider reexporting the various Text and ByteString
builder classes.

```haskell
-- ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString as X (ByteString)

-- Text
import Data.Text as X (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.IO
import Text.Printf as X (printf)

import Data.Text.Lazy (
    toStrict
  , fromStrict
  )

import Data.String.Conv as X (
    strConv
  , toS
  , toSL
  , Leniency(..)
  )

-- Printf
import Text.Printf as Exports (
    PrintfArg
  , printf
  , hPrintf
  )
```

```haskell
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString
```

#### IO

IO is essentially obvious, so we reexport the various IO operations and command
low-level command line option and file handler functions.

```haskell
-- IO
import System.Exit as X
import System.Environment as X (getArgs)
import System.IO as X (
    Handle
  , hClose
  )
```

In a just world we'd have a generic ``putStr`` that didn't require us to put
boilerplate imports in every damn module just to print a string, but alas we
don't live in that world. But we can implement a basic class to do this for the
common text representation and then just export the class instead of the four
libraries that all provide the same interface.

```haskell
class Print a where
  putStr :: MonadIO m => a -> m ()
  putStrLn :: MonadIO m => a -> m ()

instance Print T.Text where
  putStr = liftIO . T.putStr
  putStrLn = liftIO . T.putStrLn

instance Print TL.Text where
  putStr = liftIO . TL.putStr
  putStrLn = liftIO . TL.putStrLn

instance Print BS.ByteString where
  putStr = liftIO . BS.putStr
  putStrLn = liftIO . BS.putStrLn

instance Print BL.ByteString where
  putStr = liftIO . BL.putStr
  putStrLn = liftIO . BL.putStrLn

instance Print [Char] where
  putStr = liftIO . Prelude.putStr
  putStrLn = liftIO . Prelude.putStrLn

-- For forcing type inference
putText :: MonadIO m => T.Text -> m ()
putText = putStrLn
{-# SPECIALIZE putText :: T.Text -> IO () #-}

putLText :: MonadIO m => TL.Text -> m ()
putLText = putStrLn
{-# SPECIALIZE putLText :: TL.Text -> IO () #-}
```

Like the functions above we automatically lift print and the various string IO
operations into a generic ``MonadIO`` instance so we can embed them in
transformer stacks.

```haskell
print :: (X.MonadIO m, P.Show a) => a -> m ()
print = liftIO . P.print
```

#### Concurrency

The basic thread primitives are common enough that they should be brought into
scope implicitly. The ``async`` library provides the preferred method of
interacting with concurrent logic and so we export this wholesale as well.

```haskell
-- ST
import Control.Monad.ST as ST

-- Concurrency and Parallelism
import Control.Exception as X
import Control.Concurrent as X
import Control.Concurrent.Async as X
```

<hr/>

Now in our cabal file we can just add:

```haskell
default-extensions:
  NoImplicitPrelude
```

And then include your sensible Prelude in your modules. You can escape hatch out
of this choice on a module-by-module basis by just pulling in Prelude explicitly
using ``import Prelude``.

So that's the basic proto-prelude I use for my large multi-cabal-file projects.
Your mileage by vary on some of these choices, but I consider this a pretty
sensible and practical foundational set of functions and types that should at
least provide a good starting point.
