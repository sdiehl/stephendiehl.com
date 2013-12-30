---
title: Agda Tutorial
date: November 25, 2013
---

### Agda Tutorial

There wasn't much information on about getting started from first
principles with Agda. So I figured I'd write a quick intro guide to
getting started hacking with Agda. Agda is a very deep subject so this
only scratch the surface. This is a work in progress.

Assuming you have the necessary Haskell and Cabal infrastructure in
place you can run:

```bash
$ cabal install agda agda-executable
```

Then download and install the standard library to ``AgdaLibrary`` or
yoru directory of choice.

```bash
$ wget http://www.cse.chalmers.se/~nad/software/lib-0.7.tar.gz
$ tar xzf lib-0.7.tar.gz
$ mkdir ~/AgdaLibrary
$ cp -R lib-0.7/src ~/AgdaLibrary
```

For a simple complete example, create a file ``Fibonacci.agda`` with the contents:

```haskell
module Fibonacci where

open import Data.Nat

fib : ℕ → ℕ
fib 0 = 0
fib 1 = 1
fib (suc (suc n)) = fib (suc n) + fib n
```

Verify that it type checks and compiles by running.

```bash
$ agda --compile --include-path="~/AgdaLibrary" --include-path="." Fibonacci.agda
```

### Interactive Editing

If you're an Emacs user then you're in luck, simply run:

```bash
agda-mode setup
```

And add the following lines to your ``~/.emacs`` config.

```scheme
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(setq agda2-include-dirs
      (list "." (expand-file-name "~/AgdaLibrary/")))

(require 'agda2)
```

If you're a Vim addict like me and need to use vim  keybindings because they have been burned into your
psyche, then add the following to your emacs config.

```scheme
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
```

Then start emacs and execute
( <kbd class="light">Meta</kbd> <kbd class="light">x</kbd>
+ package-install + <kbd class="light">Return</kbd> + evil ).

```scheme
(package-initialize)
(evil-mode 1)

(add-hook 'evil-insert-state-entry-hook (lambda () (set-input-method "Agda")))
(add-hook 'evil-insert-state-exit-hook (lambda () (set-input-method nil)))

(global-set-key (kbd "C-c ,") 'agda2-goal-and-context)
(global-set-key (kbd "C-c .") 'agda2-goal-and-context-and-inferred)
(global-set-key (kbd "C-c C-@") 'agda2-give)
```

Evil mode will be installed and can be enabled by appending the following to your .emacs config once the
package has been installed.

Then create a empty File "Interactive.agda" and open it with Emacs.

```bash
$ emacs Interactive.agda
```

Start by entering the following following module:

```haskell
module Interactive where
```

Load the code into Emacs session by pressing ( <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">l</kbd> ). Get used to this shortcut, you'll be spamming it
  repeatedly!

Agda uses unicode prolifically and Emacs can automatically translate some common latex shortcuts to unicode
when a word is prefixed with a TeX style backslash:

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
  suc : ℕ → ℕ
```

For symbols that are not shortcut bound you can press 
( <kbd class="light">Ctrl</kbd> <kbd class="light">x</kbd>
+ <kbd class="light">8</kbd> <kbd class="light">Return</kbd> )
and tab complete to see the available list of symbols.

Now to test the interactive mode, enter the following code into the buffer.

```haskell
id : {A : Set} → A → A
id a = ?
```

Loading this with (
<kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">l</kbd> )
will tell agda to replace the
Hole `?` with a anonymous metavariable ``{ }0``. This is the
value we aim to determine. Highlighting the hole and typing
<kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">,</kbd> )
will show the corresponding goals needed to finish the program in the
bottom buffer.

```bash
Goal: .A
————————————————————————————————————————————————————————————
a  : .A
.A : Set
```

Of course the id function expression can only inhabited by one
value, namely ``a``.

We can attempt to substitute a value into the hole by pressing
( <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">Space</kbd> )
which will prompt you for a value. For
example if we attempt to provide ``zero`` we will get the
following type error:

```bash
ℕ !=< .A of type Set
when checking that the expression zero has type .A
```

For cases this like this where the value is sufficiently
constrained by the type agda can often automatically fill out the
value by using the Auto tactic. Highlight the hole and fill and
press
( <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">a</kbd> )
to attempt to auto complete the implementation.

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

Highlighting the hole and pressing
( <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">,</kbd> )
again shows us the proof we need to complete.

```bash
Goal: Bool
————————————————————————————————————————————————————————————
b : Bool
a : Bool
```

Highlighting the hole again and pressing
( <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd> )
will let us introduce a case for the first variable on the left hand
side, in this case ``b`` first..

```haskell
and : Bool -> Bool -> Bool
and a false = { }0
and a true = { }1
```

We know have two hole for the each case. Highlighting the first
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

The usual inductive definition of natural numbers has a zero and a successor function.

```haskell
data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ
```

Now we wish to define an addition over natural numbers.

```haskell
add : ℕ -> ℕ -> ℕ
add x y = { }0
```

We can stub out our addition function for naturals and let Agda fill in
the cases by pressing
( <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">,</kbd> )
and specifying the pattern variable
( <kbd class="light">x</kbd> ).

```haskell
add : ℕ -> ℕ -> ℕ
add zero y = { }0
add (suc x) y = { }1
```

The first pattern can be inferred automatically, but the second follows the normal inductive definition of
repeatedly applying sucessors.

```haskell
add : ℕ -> ℕ -> ℕ
add zero y = y
add (suc x) y = suc (add x y)
```

This can also be written equivalently by specifying an infix operator.

```haskell
_+_ : ℕ -> ℕ -> ℕ
x + zero = x
x + (suc y) = suc (x + y)
```

#### Builtins

To be written...

```haskell
{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC suc #-}
```

#### Type Signatures

```haskell
I : {A : Set} → A → A
I x = x

K : {A : Set} → {B : A → Set} → (x : A) → B x → A
K x y = x

S : {A : Set} → {B : A → Set} → {C : (x : A) → B x → Set}
  → (g : (x : A) → (y : B x) → C x y)
    → (f : (x : A) → B x)
      → (x : A)
        → C x (f x)
S g f x = g x (f x)
```

#### Totality

To be written...

#### Postulates

To be written...

#### Metavariables and Implicit Arguments

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

Just like at the value level, we can use the underscore at the type
level to let Agda deduce the type of the parameter.

```haskell
id2 : {A : _} (a : A) → A
id2 a = a
```

This occurs enough in Agda that it has its own syntatic sugar (``∀``).

```haskell
id2 : ∀ {A} (a : A) -> A
id2 a = a
```

#### Records

Record types make it possible to combine values together in a single
structure. For instance we could define a 2-tuple constructor (``_×_``)
with the following type.

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

The product operator can be entered as "\\times".

Unlike in Haskell the type of a field of record types can depend on the
values of the previosly defined fields of the same record.

To be written...

#### Pattern Matching and (⊥,⊤)

There are two special types within the Agda type system. The first (⊥) pronounced "bottom" is the type inhabited
by no values. Conversly the type (⊤) pronounced "top" is only inhabited by one value (up to isomorphism), the
value of which we'll call "unit".

To be written...

#### Proofs

Agda can inform us of the constraints on variables in scope by pressing
( <kbd class="light">Ctrl</kbd> <kbd class="light">c</kbd>
+ <kbd class="light">Ctrl</kbd> <kbd class="light">,</kbd> ).

To be written...

```haskell
open import Data.Nat
open import Data.Bool

even : ℕ → Bool
even zero = true
even (suc zero) = false
even (suc (suc n)) = even n
```

Stuff about the Curry-Howard Isomorphism to be written...

```haskell
T : Bool → Set
T true  = ⊤
T false = ⊥

F : Bool → Set
F true  = ⊥
F false = ⊤
```

To be written...

```haskell
proof : (n : ℕ) → {_ : T (even n)} → ℕ  
proof n = n * n
```

#### Relations and Equivalences

To be written...

```haskell
data _≡_ {A : Set} : A → A → Set where 
  refl : { x : A } → x ≡ x
```

```haskell
nequal : {n : ℕ} → n ≡ n
nequal = refl
```

```haskell
xequal : {x : Set} {x : A} → x ≡ x
xequal = refl
```

```haskell
cong : {m n : ℕ} → m ≡ n → 1 + m ≡ 1 + n
cong refl = refl
```

```haskell
+-assoc : ∀ m n o → m + n + o ≡ m + (n + o)
+-assoc zero n o    = refl
+-assoc (suc m) n o = cong suc (+-assoc m n o)
```

#### Type Levels and Universe Polymorphism

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

#### Module System

Unlike Haskell multiple modules can be defined in a single file

```
module MyModule where
  -- ...
```

Module can also be parameterized by arguments which are propogated throughout the body of the module
definition.

```haskell
module MyModule {foo : foo-type} (bar : bar-type) where
  -- ...
```

To use a parameterized module we can open it with a given set of parameters within the current scope.

```haskell
open MyModule foo-value bar-value
```

#### Π-Types and Σ-Types

```haskell
data Σ (A : Set) (B : A -> Set) : Set where
  _,_ : (a : A) -> B a -> Σ A B
```

Dependent sums:

```haskell
record Σ {a b} (A : Set a) (P : A → Set b) : Set (max a b) where
  constructor _,_
  field
    prj₁ : A
    prj₂ : P (prj₁)
```

To be written...

```haskell
curry : ∀ {a b c} {A : Set a} {B : A → Set b} {C : Σ A B → Set c} →
        ((p : Σ A B) → C p) →
        ((x : A) → (y : B x) → C (x , y))
curry f x y = f (x , y)
```

To be written...

#### Example: Semigroups

When coding abstract algebra in Agda we generally are working with a set called the *carrier* and a set of
operations defined over the set given the structure of the laws in interactions between between operations.

One of the simplest to implement is that of a *Semigroup*, which is a set equiped with an associative binary
operation. This is particularly nice example since it looks almost like the definition found in your abstract
algebra textbook!

```haskell
record Semigroup : Set₁ where
  field
    carrier : Set
    _∙_     : carrier → carrier → carrier
    ∙-assoc : ∀ x y z → x ∙ y ∙ z ≡ x ∙ (y ∙ z)
```

We can then instantiate an instance given our definition of natural numbers and our proof of the associativity
of ``(+)`` over them.

```haskell
NatSemigroup : Semigroup
NatSemigroup = record
  { carrier = ℕ
  ; _∙_     = _+_
  ; ∙-assoc = +-assoc
  }
```

#### Example: Categories

The most basic structure in category theory is a category which is an algebraic structure of objects (``Obj``)
and morphisms (``Hom``) with the structure that morphisms compose associatively and the existence of a identity
morphism for each object.

To be written...

```haskell
open import Level
open import Data.Product
open import Relation.Binary
open import Relation.Binary.PropositionalEquality

module Category .{o ℓ} {Obj : Set o}(Hom : Rel Obj ℓ) where
```

To be written...

```haskell
_∘_ : Set _
_∘_ = ∀ {A B C} → Hom B C → Hom A B → Hom A C
```

To be written...

```haskell
id : Set _
id = ∀ {A} → Hom A A
```

To be written...

```haskell
assoc : _∘_  → Set _
assoc _∘_ = ∀ {A B C D}(f : Hom A B)(g : Hom B C)(h : Hom C D)
          → ((h ∘ g) ∘ f) ≡ (h ∘ (g ∘ f))
```

To be written...

```haskell
leftident : id → _∘_ → Set _
leftident id _∘_ = ∀ {A B}(f : Hom A B) → ((id ∘ f) ≡ f)

rightident : id → _∘_ → Set _
rightident id _∘_ = ∀ {A B}(f : Hom A B) → ((f ∘ id) ≡ f)

identity : id → _∘_ → Set _
identity id ∘ = (leftident id ∘) × (rightident id ∘)
```
