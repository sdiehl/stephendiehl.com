---
title: The Unholy Union of Python and Haskell through LLVM
date: Aug 24, 2012
---

### The Unholy Union of Python and Haskell through LLVM

This is a crazy experiment in trying to import GHC generated LLVM bitcode directly into the Python runtime.

See the code here: [https://github.com/sdiehl/bitey-haskell](https://github.com/sdiehl/bitey-haskell)

You'll need a lot of bleeding edge stuff to even try and attempt this.

Depending on your system you'll probably need to compile LLVM, GHC, and llvm-py from source. If you don't feel like spending up 5 or 6 hours hours of your life to building a compiler pipeline then best turn away now!

-  LLVM 3.1
-  Head of the GHC source tree
-  llvm-py linked against LLVM 3.1
-  Bitey ( should be easy to install after llvm-py )
-  Optional: Bryan O'Sullivan's llvm codegen library linked against LLVM 3.1

#### Compiling LLVM

You need the latest LLVM 3.1. You'll probably need to compile from source or grab the binaries for your platform and sub in the path in the steps below.

* http://llvm.org/releases/

#### Compiling the Glorious Haskell Compiler

The LLVM backend supposedly works well on Linux and BSD derivatives but I have not tested it on anything other than x86 Linux. When trying this out my platform was:

* Arch Linux x86_64
* gcc (GCC) 4.6.1 20110819 (prerelease)

```bash
$ git clone http://darcs.haskell.org/ghc.git/
$ cd ghc
$ ./sync-all --testsuite get
$ perl boot
```

Then you'll need to create ``mk/build.mk`` . Note that we're turning off optimization of the Stage 2 build so performance will suck.

```bash
SRC_HC_OPTS     = -O -H64m
GhcStage1HcOpts = -O -fllvm
GhcStage2HcOpts = -O0 -fasm
GhcHcOpts       = -Rghc-timing
GhcLibHcOpts    = -O0
GhcLibWays     = v
GhcWithLlvmCodeGen = YES

ifeq "$(PlatformSupportsSharedLibs)" "YES"
GhcLibWays += dyn
endif
```

Note: Both ``opt`` and ``llc`` must be on your PATH!

Then build. That will take a few hours.

```
$ ./configure
$ make
```

After you've compiled it and subsequently increased the entropy of the universe, you should now have a fresh ``ghc-cabal`` and ``ghc-stage1`` inside of the ``inplace`` folder. Let's try out the new compiler. Make a hello.hs with the following

```haskell
import System.IO

main = do
    putStrLn "Hello World"
```

And compile and run:

```bash
$ inplace/bin/ghc-stage1 --make -fllvm hello.hs
$ ./hello
Hello World
```

If you check the info of your new compiler it should look something like this:

```bash
$ inplace/bin/ghc-stage1 --inf
  [("Project name","The Glorious Glasgow Haskell Compilation System")
 ,("GCC extra via C opts"," -fwrapv")
 ,("C compiler command","/usr/lib/colorgcc/bin/gcc")
 ,("C compiler flags"," -fno-stack-protector  -Wl,--hash-size=31 -Wl,--reduce-memory-overheads")
 ,("ld flags","     --hash-size=31     --reduce-memory-overheads")
 ,("ld supports compact unwind","YES")
 ,("ld supports build-id","YES")
 ,("ld is GNU ld","YES")
 ,("ar command","/usr/bin/ar")
 ,("ar flags","q")
 ,("ar supports at file","YES")
 ,("touch command","touch")
 ,("dllwrap command","/bin/false")
 ,("windres command","/bin/false")
 ,("perl command","/usr/bin/perl")
 ,("target os","OSLinux")
 ,("target arch","ArchX86_64")
 ,("target word size","8")
 ,("target has GNU nonexec stack","True")
 ,("target has .ident directive","True")
 ,("target has subsections via symbols","False")
 ,("Unregisterised","NO")
 ,("LLVM llc command","/usr/bin/llc")
 ,("LLVM opt command","/usr/bin/opt")
 ,("Project version","7.7.20120822")
 ,("Booter version","7.4.2")
 ,("Stage","1")
 ,("Build platform","x86_64-unknown-linux")
 ,("Host platform","x86_64-unknown-linux")
 ,("Target platform","x86_64-unknown-linux")
 ,("Have interpreter","YES")
 ,("Object splitting supported","YES")
 ,("Have native code generator","YES")
 ,("Support SMP","YES")
 ,("Tables next to code","YES")
 ,("RTS ways","l debug  thr thr_debug thr_l  dyn debug_dyn thr_dyn thr_debug_dyn")
 ,("Leading underscore","NO")
 ,("Debug on","False")
 ,("LibDir","/home/stephen/Git/ghc/inplace/lib")
 ,("Global Package DB","/home/stephen/Git/ghc/inplace/lib/package.conf.d")
 ]
```

There is a ghc wrapper in the ghc-llvm folder. Ostensibly its a modified version of Don Stewarts ghc-core except it dumps the LLVM IR output for a a given file you pass to GHC. To install and use:

```bash
$ cd ghc-llvm
$ cabal install

# To use
$ ghc-llvm hello.hs

%__stginit_Main_struct = type <{}>
@__stginit_Main =  global %__stginit_Main_struct<{}>
%sfR_srt_struct = type <{i64}>
@sfR_srt = internal constant %sfR_srt_struct<{i64 ptrtoint ([0 x i64]* @ghczmprim_GHCziCString_unpackCStringzh_closure to i64)}>
@ghczmprim_GHCziCString_unpackCStringzh_closure = external global [0 x i64]
%sfR_closure_struct = type <{i64, i64, i64, i64}>

...

# or
$ ghc-llvm hello.hs > hello.bc
```

Its also worth noting that GHC lets you customize the path locations of ``opt `` and ``llc``.

```
-pgmlo - The program to use as the llvm optimiser
-pgmlc - The program to use as the llvm compiler
```

#### Installing LLVM-PY

Now you need to install llvm-py.

```bash
$ git clone https://github.com/llvmpy/llvmpy.git
$ python setup.py --llvm-config=<PATH TO LLVM 3.1>
```

Test it with:

```python
$ python2
>>> import llvm
>>> from llvm import *
```

#### Importing with Bitey

Then modify the ``Makefile`` to reflect your system paths.

If everything works ( big if! ) you should then be able to run:

```bash
$ make

... lots of intermediate output ...
```

Now create a ``Test.hs`` module.

```haskell
{-# Language MagicHash #-}

import GHC.Prim

f :: Int# -> Int#
f = (-# 1#)

g :: Int# -> Int#
g = (-# 2#)

fib :: Int# -> Int#
fib (0#) = 0# 
fib (1#) = 1#
fib n = (+#) (f n) (g n)

main = do
    return ()
```

Which should generate the LLVM bitcode for the Haskell module ``Test.hs``. Cross your fingers and try and load it into bitey!

```python
$ cd bitey-haskell
$ python2
>>> import bitey
>>> Test = bitey.load_library('./bitey.o')
>>> Test.f(3)
2
>>> Test.g(5)
3
>>> Test.fib(8)
21
```

Note: You'll need to use the provided ``haskell-bitey`` Vanilla bitey won't work with the Haskell flavored module.
