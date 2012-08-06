I'm available for consulting and possibly full-time positions in the
Cambridge/Boston area.  Let's talk about your idea and what I can do.

I don't respond to third party recruiter emails. I'm also 
completely uninterested in any projects related to "social media".

To contact me via email run this Python script to generate my contact
information. If you can't get it to run then you're probably not the
kind of person I want to work for.

```python
#!/usr/bin/env python
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

