---
title: Stephen Diehl boston python developer gevent
---

I'm available for consulting and possibly full-time positions in the
Cambridge/Boston area.  Let's talk about your idea and what I can do.

To contact me via email run this Python script to generate my contact
information. I find this is an effective filter to find the kind
of people I'd like to work with. 

Hint: If you're using IPython use the %paste magic method.

```python
# Generate contact information for Stephen Diehl
# http://www.stephendiehl.com

import os
import sys
import zlib
import numpy
import string

EMAIL = """
\xcd\xce\xbf\xca\xc2\xbf\xc8.\xc7.\xbe\xc3\xbf\xc2\xc6@\xc1\xc7\xbb\xc3\xc6.\xbd\xc9\xc7
"""
CHECKSUM = 0x87980992

y = string.letters
n = len(y)
M = numpy.identity(n, dtype=numpy.int16)
M[:n, n-1] = 1

a = numpy.array(map(ord, y), dtype=numpy.int16)
x = ''.join(map(chr, numpy.dot(M, a)))
uncipher = string.maketrans(x,y)

CONTACT = string.translate(EMAIL, uncipher)

assert zlib.adler32(CONTACT) & 0xffffffff == CHECKSUM, "Wrong"
sys.stdout.write(CONTACT)
```

