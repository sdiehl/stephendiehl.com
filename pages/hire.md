---
title: Stephen Diehl Contact
---

To contact me via email run one of the following scripts to
generate my contact information. I find this is an effective
filter against the deluge of emails from recruiters.

I am currently available for hire.

**Haskell**

```haskell
-- Generate contact information for Stephen Diehl
-- http://www.stephendiehl.com

import Data.Char (ord, chr)
import Data.Ix (inRange)
 
cipher :: Int -> String -> String
cipher k = map f
  where
    f c
      | inRange ('a','z') c = tr 'a' k c
      | inRange ('A','Z') c = tr 'A' k c
      | otherwise = c
 
uncipher :: Int -> String -> String
uncipher k = cipher (-k)
 
tr :: Char -> Int -> Char -> Char
tr base offset char = chr $ ord base + (ord char - ord base + offset) `mod` 26

main :: IO ()
main = putStrLn $ uncipher 1 "tufqifo.n.ejfim@hnbjm.dpn"
```

**Python**

```python
# Generate contact information for Stephen Diehl
# http://www.stephendiehl.com

import os
import sys
import numpy
import string

if sys.version > '3':
    strl = str
else:
    strl = string

EMAIL = """
\xcd\xce\xbf\xca\xc2\xbf\xc8.\xc7.\xbe\xc3\xbf\xc2\xc6@\xc1\xc7\xbb\xc3\xc6.\xbd\xc9\xc7
"""

y = string.ascii_letters
n = len(y)
M = numpy.identity(n, dtype=numpy.int32)
M[:n, n-1] = 1

a = numpy.array(list(map(ord, y)), dtype=numpy.int32)
x = ''.join(map(chr, numpy.dot(M, a)))
uncipher = strl.maketrans(x,y)

print( strl.translate(EMAIL, uncipher) )
```
