---
title: Agda Tutorial
date: Jun 15, 2012
---

### Agda Tutorial

There wasn't much information on about bootstrapping
an Agda installation so I figured I'd write a quick guide.
I'm assuming you have the necessary Haskell infrastructure in
place.

```bash
$ cabal install agda agda-executable

```

For integration with Emacs run

```bash
agda-mode setup
```

\

Then download and install the standard library to ``AgdaLibrary`` or
yoru directory of choice.

```bash
$ wget http://www.cse.chalmers.se/~nad/software/lib-0.7.tar.gz
$ tar xzf lib-0.7.tar.gz
$ mkdir ~/AgdaLibrary
$ cp -R lib-0.7/src ~/AgdaLibrary
```

\

For a simple complete example, create a file ``Fibonacci.agda`` with the contents:

```haskell
module Fibonacci where

open import Data.Nat

fib : ℕ → ℕ
fib 0 = 0
fib 1 = 1
fib (succ (succ n)) = fib (succ n) + fib n
```

```bash
$ agda --compile --include-path="~/AgdaLibrary" --include-path="." Fibonacci.agda
```

Veriy that it type checks and compiles.

### Interactive Editing

Create a empty File "Interactive.agda" and open it with Emacs.

```bash
$ emacs Interactive.agda
```

Start by entering the following following module:

```haskell
module Interactive where
```

Load the code into interactive session by Pressing ``Ctrl-c
Ctrl-l``.

Agda uses unicode prolifically and Emacs can automaticlly
translate some common latex shortcuts to unicode:

<table>
<tr>
<td>``\to``</td>
<td>→</td>
</tr>
<tr>
<td>``\bn``</td>
<td>ℕ</td>
</tr>
<tr>
<td>``\all``</td>
<td>∀</td>
</tr>
</table>


```haskell
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ
```

For symbols that are not shortcut bound you can press ``Ctrl-x 8
Return`` and tab complete to see the available list of symbols.

Now to use the interactive mode, enter the following coe.

```haskell
id : {A : Set} → A → A
id a = ?
```

Loading this with ``Ctrl-c Ctrl-l`` will tell agda to replace the
Hole `?` with a anonymous metavariable ``{ }0``. This is the
value we aim to determine. Highlighting the hole and typing
``Ctrl-c Ctrl-,`` will show the corresponding goals needed to
finish the program.


```bash
Goal: .A
————————————————————————————————————————————————————————————
a  : .A
.A : Set
```

Of course the id function expression can only inhabited by one
value, namely ``a``.

We can attempt to substitute a value into the hole by pressing
``Ctrl-c Ctrl-Space`` which will prompt you for a value. For
example if we attempt to provide ``zero`` we will get the
following type error:

```bash
ℕ !=< .A of type Set
when checking that the expression zero has type .A
```

For cases this like this where the value is sufficiently
constrained by the type agda can often automatically fill out the
value by using the Auto tactic. Highlight the hole and fill and
press ``Ctrl-c Ctrl-a`` to auto complete the implementation.

Now we define our Bool type.

```haskell
data Bool : Set where
  false : Bool
  true  : Bool
```

To write down an `and` function there are two combinations of
arguments we have to deal with, namely `true false` and `false
true`. Agda knows the types of the arguments and can write most
of our function for us.

```haskell
and : Bool -> Bool -> Bool
and a b = ?
```

Highlighting the hole and pressing ``Ctrl-c Ctrl-,`` shows us the
proof we need to complete.

```bash
Goal: Bool
————————————————————————————————————————————————————————————
b : Bool
a : Bool
```

Highlighting the hole again and pressing ``Ctrl-c Ctrl-c`` will
let us introduce a case for the first variable on the left hand
side, in this case ``b`` first..

```haskell
and : Bool -> Bool -> Bool
and a false = { }0
and a true = { }1
```

We know how two hole for the each case. Highlighting the first
and doing the same procedure will the fill in the cases for a.

```haskell
and : Bool -> Bool -> Bool
and false false = { }0
and false true = { }1
and true false = { }2
and true true = { }3
```

We need then only fill in the right hand sides.

```haskell
and : Bool -> Bool -> Bool
and false false = false
and false true = false
and true false = false
and true true = true
```

#### Inductive Definitions

We have the definition of natural number

```haskell
data ℕ : Set where
  zero : ℕ
  succ  : ℕ → ℕ
```

We wish to define an addition over natural numbers.

```haskell
add : ℕ -> ℕ -> ℕ
add x y = { }0
```

We can stub out our addition function for naturals and let Agda fill in
the cases by pressing ``Ctrl-c Ctrl-c`` and specifying the pattern variable
``x``.

```haskell
add : ℕ -> ℕ -> ℕ
add zero y = { }0
add (succ x) y = { }1
```

The first pattern can be inferred automatically, but the second follows the normal inductive definition of
repeatedly applying successors.

```haskell
add : ℕ -> ℕ -> ℕ
add zero y = y
add (succ x) y = succ (add x y)
```

This can also be written equivalently by specifying an infix operator.

```haskell
_+_ : ℕ -> ℕ -> ℕ
x + zero = x
x + (succ y) = succ (x + y)
```

#### Implicit Arguments

Agda of course allows us to specify functions which span multiple types. For example the if-then-else blocks
can be written in Agda as the following de sugared function.

```haskell
ife : (A : Set) -> Bool -> A ->	A -> A
ife t true x y = x
ife t false x y = y
```

The value `(A : Set)` specifies that the type of the A to bind over the
the variable A. This is of course trivial for the type checker to infer
at call site so we can instead write it as an implicit parameter and let
the type checker do just that.

```haskell
ife' : {A : Set} -> Bool -> A -> A -> A
ife' true x y = x
ife' false x y = y
```

#### Records

Record types make it possible to combine values together in a single structure. For instance we could define a
2-tuple constructor (``_×_``) with the following type:

```haskell
record _×_ (A B : Set) : Set where
  field
    first : A
    second : B

fst : ∀ {A B} → A × B → A
fst = _×_.first

snd : ∀ {A B} → A × B → B
snd = _×_.second
```

#### Universe Levels and Polymorphism

To be written...

```haskell
data Level : Set where
  zero : Level
  suc  : (i : Level) → Level
```

```haskell
{-# BUILTIN LEVELZERO zero  #-}
{-# BUILTIN LEVELSUC  suc   #-}
```

```haskell
infixl 6 _⊔_

_⊔_ : Level → Level → Level
zero  ⊔ j     = j
suc i ⊔ zero  = suc i
suc i ⊔ suc j = suc (i ⊔ j)
```

#### Π-Types and Σ-Types

To be written...

#### Relations and Proofs

To be written...

#### Example: Category

To be written...

```haskell
open import Level
open import Relation.Binary
open import Relation.Binary.PropositionalEquality

open import Data.Product

module Category .{o ℓ} {Obj : Set o}(Hom : Rel Obj ℓ) where

(∘) : Set _
(∘) = ∀ {A B C} → Hom B C → Hom A B → Hom A C

(id) : Set _
(id) = ∀ {A} → Hom A A
```

To be written...

```haskell
assoc : (∘) → Set _
assoc _∘_ = ∀ {A B C D}(f : Hom A B)(g : Hom B C)(h : Hom C D)
          → ((h ∘ g) ∘ f) ≡ (h ∘ (g ∘ f))
```

To be written...

```haskell
leftident : (id) → (∘) → Set _
leftident id _∘_ = ∀ {A B}(f : Hom A B) → ((id ∘ f) ≡ f)

rightident : (id) → (∘) → Set _
rightident id _∘_ = ∀ {A B}(f : Hom A B) → ((f ∘ id) ≡ f)

identity : (id) → (∘) → Set _
identityt id ∘ = leftident id ∘ × rightident id ∘
```
