---
title: Tipy: A Simple Python Preprocessor
date: August 6, 2012
---

Tipy is a small Python document preprocessor in the style
of troff. It is similar to that of Ned Batchelder's
[Cog](http://nedbatchelder.com/code/cog/) but not as fully featured and
intended only for preprocessing markdown documents.

**Problem**: You have a build chain of a bunch of unix utilities operating on
a plaintext document, and you have Python code where you want
inline the output of the code. In my case I wanted to provide
this to Haskell Pandoc + Hakyll build chain. In fact this
page is itself preprocessed with tipy.

<pre>
```compiler
>>> def foo(x,y):
...     return range(x,y)
>>> foo(1,2)
```
</pre>

For writing tutorials one often wants ot mimic an interactive
shell session. This is provided through the ``compiler=pycon`` flag.

```python
>>> def foo(x,y):
...     return range(x,y)
>>> foo(1,2)
```

But you want the compiled version to look like it were an
interactive shell session.

```pycon
>>> def foo(x,y):
...     return range(x,y)

>>> foo(1,5)
```

\

Or if desired you can execute the entire script with the 
``compiler=pyexec`` flag and inline the output of the whole
script.

```pyexec
from string import digits
from itertools import izip

for a,b in izip(reversed(digits), digits):
    print a,b
```

### More Examples

```pyexec
from numpy.random import randn

matrix = randn(3,3,3)
print matrix
```

```pycon
>>> from pandas import DataFrame
>>> from numpy.random import randn
pragma:dataframe
>>> DataFrame(randn(5,5))
```

### Integration with Hakyll

```haskell
import Hakyll

-- Python tipy preprocessor
py_pre :: Compiler Resource String
py_pre = getResourceString >>> unixFilter "tipy" ["--preprocess"]

main :: IO ()
main = hakyll $ do
    match "yourpage" $ do
        compile $ py_pre
            >>> arr readPage
            >>> pageRenderPandoc
```
