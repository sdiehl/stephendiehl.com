StephenDiehl.com
================

Stephen Diehl's website.

Build Directions
----------------

For typos and fixes edit the markdown files here:

[/posts/](https://github.com/sdiehl/stephendiehl.com/tree/master/posts)

To build the website server use Stack or Cabal:

**Stack**

```bash
$ stack ghc hakyll
$ stack exec ./hakyll build
```

**Cabal**

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal run
```

```bash
$ make hakyll
$ make
```

License
-------

Copyright (c) 2012-2018, Stephen Diehl

All written content on this site is provided under a Creative Commons ShareAlike
license. All code is provided under a MIT license unless otherwise stated.
