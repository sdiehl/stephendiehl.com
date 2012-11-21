---
title: Agda Quickstart Guide
date: Jun 15, 2012
---

### Agda Quickstart Guide

There wasn't much information on about bootstrapping
an Agda installation so I figured I'd write a quick guide.
I'm assuming you have the neccesary Haskell infastructure in
place.

```bash
$ cabal install agda agda-executable

```

\

Then download and install the standard library.

```bash
wget http://www.cse.chalmers.se/~nad/software/lib-0.6.tar.gz
tar xzf lib-0.6.tar.gz
mkdir ~/.agda
cp -R lib-0.6/src ~/.agda
```

\

The Hello World program for Agda. Suprisingly printing to the screen is
non-trivial, but logic is not!

```haskell
module HelloLogic where
 
open import Data.Bool
open import Data.Product
 
test : Bool → Bool → Bool × Bool × Bool
test x y = x ∧ y , x ∨ y , not x
```

```bash
$ agda --compile --include-path=".agda/" --include-path="." HelloLogic.agda
```

It then type checks. Your work here is done!
