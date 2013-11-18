---
title: Agda Tutorial
date: Jun 15, 2012
---

### Agda Tutorial

There wasn't much information on about bootstrapping
an Agda installation so I figured I'd write a quick guide.
I'm assuming you have the neccesary Haskell infastructure in
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

Create a file ``Fibonacci.agda`` with the contents:

```haskell
module Fibonacci where

open import Data.Nat

fib : ℕ → ℕ
fib 0 = 0
fib 1 = 1
fib (suc (suc n)) = fib (suc n) + fib n
```

```bash
$ agda --compile --include-path="~/AgdaLibrary" --include-path="." Fibonacci.agda
```

It type checks.

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
  suc  : ℕ → ℕ
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
and doign the same procedure will the fill in the cases for a.

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
