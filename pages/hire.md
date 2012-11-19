---
title: Stephen Diehl Contact
---

To contact me via email run the following Python script to generate my
contact information. I find this is an effective filter against the
deluge of emails from recruiters.

I'm not available for hire.

Hint: If you're using IPython use the %paste magic method.

```python
# Generate contact information for Stephen Diehl
# http://www.stephendiehl.com

import os
import sys
import numpy
import string

EMAIL = """
\xcd\xce\xbf\xca\xc2\xbf\xc8.\xc7.\xbe\xc3\xbf\xc2\xc6@\xc1\xc7\xbb\xc3\xc6.\xbd\xc9\xc7
"""

y = string.letters
n = len(y)
M = numpy.identity(n, dtype=numpy.int16)
M[:n, n-1] = 1

a = numpy.array(map(ord, y), dtype=numpy.int16)
x = ''.join(map(chr, numpy.dot(M, a)))
uncipher = string.maketrans(x,y)

CONTACT = string.translate(EMAIL, uncipher)
sys.stdout.write(CONTACT)
```
