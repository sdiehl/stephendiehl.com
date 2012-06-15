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

Then to compile a file called ``SOURCE.agda`` execute the following
command in your build script.

```bash
$ agda --compile --include-path="~/.agda" --include-path="." SOURCE.agda
```
