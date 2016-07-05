---
title: A Sticky Stringy Quandary
date: June 4, 2016
---

### A Sticky Stringy Quandary

Anyone who has used Haskell in a professional setting knows that the String
situation is kind of a mess. While in many ways the language is progressing at a
rapid pace and is only ever getting more compelling for commercial use, the
String situation is still regarded by many people as the [largest problem in the
langauge](https://www.reddit.com/r/haskell/comments/4f47ou/why_does_haskell_in_your_opinion_suck/d25zq7t).
And for good reason, an efficient textual type is absolutely essential for most
work and it's use needs to be streamlined and language-integrated for a overall
positive experience writing industrial Haskell.

Let us a consider a logical assessment of why the **String Situation** exists,
how far we can get with workarounds and what's next. See the accompanying Git
project for prototype code:

<p style="text-align:center">
**[Accompaying Source Code](https://github.com/sdiehl/print)**
</p>

### String

The String type is very naive, it's
[defined](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-String.html#t:String)
as a linked-list of Char pointers.

```haskell
type String = [Char]
```

This is not only a bad representation, it's quite possibly the least efficient
(non-contrived) representation of text data  possible and has horrible
performance in both time and space. *And it's used everywhere in Haskell.* Even
posterchild libraries for Haskell (Pandoc, etc) use it extensively and have
horrible performance because of it.

Around 2005-2007 several more efficient libraries were written, that included
[Bytestring](https://hackage.haskell.org/package/bytestring) and
[Text](https://hackage.haskell.org/package/text) and both have different
use-cases. Both are orders of magnitude more efficient and have become the
ubiquitous in "Modern Haskell". Combined with the recent ``-XOverloadedStrings``
language extension we have a partial solution for routing around the problem.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

-- From overloaded string literal.
foo :: T.Text
foo = "bar"
```

Unfortunately conversion between the efficient string types and String is $O(n)$
and involves a deep copy. They're still not used ubiquitously, and every
introductory book on the subject still uses String instead of the modern
libraries because it's provided by default. 

So why is String still used? Because it's too convenient and it has special
powers from being wired-in to the compiler.

### Banishing String

You can get pretty far working in a subset of the Prelude and blessed libraries
that have nearly removed old historical cruft like String and banished the ugly
parts of the Prelude. However one will end up using String in few noticeable
dark corners. 

1. Show instances
1. Read instances
1. Pretty printers
1. FilePath
1. Third party libraries written before 2007.

Older core libraries are getting slowly phased out, this is a social problem not
a technology problem. This seems to be going in the right direction on it's own.

FilePaths are not hard to swap out and not a huge concern.

``Show`` typeclasses and Pretty printers are the probably the singularly biggest
source of continued [Char] usage and what we'll concern ourselves with here.

***

### Show

The Show class is really useful, and automatically deriving show much
boilerplate is part of the reason Haskell is so much fun to write. However it's
current status poses a bit of a problem transitioning to modern types for
several reasons:
  
a) It's abused to write custom pretty printers.
b) It's relation to the Read class is problematic.
c) It's constrained to use ``[Char]`` and forces that choice on downstream
   users, who end up forced to use it in places it shouldn't be used.

So what is Show class really, it's so successful that a lot of people actually
never look at it's internals. The guts of it is a function called ``showPrec``
which is a overloaded CPS'd function which composes together a collection of
string fragments for specific implementations of the Show typeclass.

```haskell
type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
```

Together with the Read class we get a textual serializer and deserializer for
free with the laws governing that relation being:

```haskell
read . show = id
```

GHC can almost always derive this automatically and the instance is pretty
simple. Using ``-ddump-deriv`` we can ask GHC to dump it out for us.

```haskell
λ> :set -ddump-deriv 
λ> data List a = Nil | Cons a (List a) deriving (Show)

instance Show a => Show (List a) where
  showsPrec _ Nil = showString "Nil"
  showsPrec a (Cons b1 b2)
    = showParen (a >= 11) $ 
           showString "Cons " 
         . showsPrec 11 b1 
         . showSpace
         . showsPrec 11 b2

  showList = showList__ (showsPrec 0)
```

The emergant problem this is that there are an enormous number of pathological
Show instances used in practice, and you don't need to look even beyond the
standard library to find law violations. This coupled with the fact that Read
instance is really dangerous, it's use of a very suboptimal String type means
that it's inefficient and opens up security holes and potential
denial-of-service attacks in networked applications. Show should really only to
be used for debugging the structure of internal types and used at the
interactive shell. For serializing structures to text in a way that differs from
Haskell's internal representation we need a pretty printer.

### Pretty Printers

The correct way of writing custom textual serializes is through the various
pretty-print combinator libraries that stem from Wadler's original paper [A
prettier
printer](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.19.635&rep=rep1&type=pdf).
There are some degrees of freedom in this design space, but
[wl-pprint-text](https://hackage.haskell.org/package/wl-pprint-text) is a good
choice for almost all use cases. Using the underlying [Data.Text.Lazy.Builder](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text-Lazy-Builder.html) functions
is also a sensible choice.

So for example if we have a little λ-calculus...

```haskell
data Expr
  = Var Text
  | Lit Lit
  | App Expr Expr
  | Lam Text Expr

data Lit
  = LInt Int
  | LBool Bool
```

We can write down a pretty printer quite simply using the provided combinators.
See the [gist
here](https://gist.github.com/sdiehl/d8eae8db0a9fa34c54c10cb46a0b2dc8) for the
full example.

```haskell
class Pretty a where
  pretty :: Int -> a -> Doc

instance Pretty Lit where
  pretty _ (LInt n) = int n
  pretty _ (LBool b) = bool b

instance Pretty Expr where
  pretty _ (Var x)  = text x
  pretty p (Lit x)  = pretty p x

  pretty p e@(App _ _) =
    let (f, xs) = viewApp e in
    let args = sep $ map (pretty (p+1)) xs in
    parensIf (p>0) $ pretty p f <+> args

  pretty p e@(Lam _ _) =
    let body = pretty (p+1) (viewBody e) in
    let vars = map text (viewVars e) in
    parensIf (p>0) $ "\\" <> hsep vars <+> "." <+> body
```


```haskell
ppexpr :: Expr -> Text
ppexpr x = PP.displayT (PP.renderPretty 1.0 70 (pretty 0 x))
```

So that's how it *should* be done. In practice to do this you'd have to setup a
cabal/stack project, install 11 dependencies, write a new typeclass, and write
this little joy of a import preamble masking several functions that conflict in
the Prelude namespace.

```haskell
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as TL
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>), Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text as PP
```

This kind of sucks. It's the right thing to do, but it's kind of painful and
it's certainly not intuitive for newcomers. Abusing Show and String is easier,
*worst practices should be hard* but in this case they are much easier than
doing the correct thing.

### Progress :: String → Text

GHC has had the capacity to support custom Preludes for a while and this is a
very wise design choice. For all the historical brokenness of certain things,
there are very few technological hurdles to replacing them with modern sensible
defaults. The question then remains how close can we come to replacing Show with
a Text-based equivalent. The answer is about 80% before surgery is required on
GHC itself.

The translation of a Text-based show prototype is just one module.  Instead of
concatenating Strings we use the Text.Builder object to build up a Text
representation. The ``ShowS`` function now just becomes a Builder
transformation.

```haskell
import Data.Text.Buildable (build)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

type ShowS = TB.Builder -> TB.Builder

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> TL.Text
  show x = TB.toLazyText (shows x "")
```

The various builtin types have builder objects implemented by Buildable that
efficiently render to Text.

```haskell
instance Show Integer where
  showsPrec _ n f = build n <> f

instance Show Float where
  showsPrec _ n f = build n <> f

instance Show Double where
  showsPrec _ n f = build n <> f

instance Show Bool where
  showsPrec _ True = showText "True"
  showsPrec _ False = showText "False"
```

For constructors with parameters there is a very mechanical translation
that is exactly like how ``deriving Show`` works for String.

```haskell
con0 :: Text -> ShowS
con0 = showText

con1 :: Show a => Text -> Int -> a -> ShowS
con1 con p x = 
  showParen (p > appPrec) $
    showText con .
    showsPrec appPrec1 x

instance Show a => Show (Maybe a) where
  showsPrec _  Nothing s = con0 "Nothing" s
  showsPrec p (Just x) s = con1 "Just " p x s
```

Since it's easy to generate the boilerplate instances, we can use Generics to
auto-derive the instance for any sum/product type expressible in Haskell and
have a DefaultSignature for ``showsPrec``.

```haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> TL.Text

  default showsPrec :: (Generic a, GShow (Rep a)) => Int -> a -> ShowS
  showsPrec i x = gshowsPrec Pref i (from x)

  show x = TB.toLazyText (shows x "")
```

And then some ugly (but mechanical) builder munging gives us an exact copy of
GHC's show format. The little known
[``-XDeriveAnyClass``](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-any-other-class)
can be used to derive any other class that has an empty minimal set or uses
DefaultSignatures and Generic instances to implement methods.


```haskell
class GShow f where
  gshowsPrec :: Type -> Int -> f a -> ShowS
  basic :: f a -> Bool
  basic _ = False

instance  GShow U1 where
  gshowsPrec _ _ _ = id
  basic _ = True

instance Show c => GShow (K1 i c) where
  gshowsPrec _ i (K1 fp) = showsPrec i fp

instance (GShow f, Constructor c) => GShow (M1 C c f) where
  gshowsPrec t i c@(M1 fp) 
    | conIsRecord c = 
          showString (conName c)
        . showChar ' '
        . showBraces (gshowsPrec Rec i fp)

    | otherwise = case conFixity c of
      Prefix -> showParen (i > appPrec && not (basic fp)) $
          showString (conName c)
        . if basic fp then id else showChar ' '
        . gshowsPrec t appPrec1 fp

      Infix _ m -> showParen (i > m) $ 
        showBraces (gshowsPrec t m fp)

instance (GShow f, Selector c) => GShow (M1 S c f) where
  gshowsPrec t i c@(M1 fp) = case t of
    Pref    -> gshowsPrec t i fp
    Inf _   -> gshowsPrec t i fp
    Rec     ->
        showString (selName c)
      . showText " = "
      . gshowsPrec t i fp

instance (GShow f) => GShow (M1 D c f) where
  gshowsPrec t i (M1 fp) = gshowsPrec t i fp

instance (GShow f, GShow g) => GShow (f :+: g) where
  gshowsPrec t i (L1 fp) = gshowsPrec t i fp
  gshowsPrec t i (R1 fp) = gshowsPrec t i fp

instance (GShow a, GShow b) => GShow (a :*: b) where
  gshowsPrec t@Rec n (a :*: b) =
      gshowsPrec t n a 
    . showText ", "
    . gshowsPrec t n b

  gshowsPrec t@Pref n (a :*: b) =
      gshowsPrec t (n+1) a 
    . showChar ' '
    . gshowsPrec t (n+1) b

  gshowsPrec t@(Inf s) n (a :*: b) =
      gshowsPrec t n a 
    . showText s 
    . gshowsPrec t n b
```

And there we have it, a fixed show function that is drop-in compatible with the
existing format but uses Text...

```haskell
show :: Show a => a -> Text
```

... and has automatic deriving.


```haskell
data List a 
  = Nil
  | Cons a (List a)
  deriving (Generic, Print.Show)
```

We can even go so far as to tell GHCi to use our custom function at the Repl by
adding the following to our projects ``.ghci`` file.

```haskell
import Print
:set -interactive-print=Print.print
```

However GHC's defaulting mechanism has a bunch of ad-hoc specializations for
wired-in classes that don't work for user-defined clases. If we type in an
under-specified expression for Show, GHC will just splice in a show dictionary
for the unit type `` Show () `` if it can't figure out an appropriate
  dictionary.

```haskell
λ> print Nothing

==================== Simplified expression ====================
bindIO
  (ghciStepIO
     $fGHCiSandboxIOIO (print ($fShowMaybe $fShow()) (Nothing)))
  (\ (it :: ()) -> returnIO (: it ([])))
```

For our implementation:

```haskell
λ> print Nothing

<interactive>:3:1:
    No instance for (Show a0) arising from a use of ‘print’
    The type variable ‘a0’ is ambiguous
    Note: there are several potential instances:
      instance (Show a, Show b) => Show (Either a b)
        -- Defined at Print.hs:233:10
      instance Show a => Show (Maybe a) -- Defined at Print.hs:229:10
      instance Show Int16 -- Defined at Print.hs:160:10
      ...plus 27 others
    In the expression: print Nothing
    In an equation for ‘it’: it = print Nothing
```

There's currently no way to do this for a custom Show type. This implementation
also requires a ``Generic`` instance and several language extensions. This is
the hard limit to how far we can go in "user space".

### Implementation

What we can prototype with Generics today is not hard to translate over into a
builtin deriving mechanism inside the compiler tomorrow.  In fact we can create
a compatibility layer so close the existing Show class deriving that we reuse
all of it's logic sans the type changes.

```haskell
module Print (
  Show(showsPrec, show),
  ShowS,
  showChar,
  showText,
  showList,
  showList__,
  showParen,
  shows,
) where
```

Right now in GHC there is a ``hasBuiltinDeriving`` that checks if the derived
class is one of the blessed "builtins" that has a prescription for deriving a
class instance for it. The blessed classes include:

**Class**   **GHC Function**
-----       ----
Eq          [gen_Eq_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L192)
Ord         [gen_Ord_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L361)
Enum        [gen_Enum_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L587)
Bounded     [gen_Bounded_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L667)
Ix          [gen_Ix_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L754)
Show        [gen_Show_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L1114)
Read        [gen_Read_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L946)
Data        [gen_Data_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L1276)
Functor     [gen_Functor_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L1553)
Foldable    [gen_Foldable_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L1870)
Traversable [gen_Traversable_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L1994)
Lift        [gen_Lift_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenDeriv.hs#L1994)
Generic     [gen_Generic_binds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcGenGenerics.hs#L66)
Typeable    [mkTypeableBinds](https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcTypeable.hs#L95)

If the public interface for generating a Text-Show instance recycled the same
structure as String version, we could very easily write ``gen_ShowText_binds``
and plug this into the compiler to derive a new (distinct) text Show that
wouldn't break compatibility.

However, at the moment ``text`` isn't in GHC's boot libraries and can't be made
into [wired-in
type](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/TysWiredIn.html)
which would be necessary to add the new deriving mechanism to ``TcGenDeriv.hs``.
So that's as far as we can go in 2016, there's probably a fairly clear path to
removing Stringy-Show if ``text`` were to at some point become accessible to GHC
internals.

<!--

### The Future

If one is working in industrial Haskell and plans to be in this space for
several decades, there's a certain set of questions The Thoughtful Haskell
programmer should consider.

1. In the year 2027 do I still want to be writing hundreds of lines of
   boilerplate Haskell in every module to route around decisions made in 1997?
2. What time is better than now to diverge from these "worst practices" and remove them
   entirely from our compiler and language instead of simply routing around the
   problem?
3. What's the minimal viable set of packages we need to move over to Modern
   Haskell to satisfy the needs of industrial users?

-->
