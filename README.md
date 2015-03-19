# SRFI 60 'Integers as Bits'

This is an implementation of [SRFI 60 'Integers as Bits'](//srfi.schemers.org/srfi-60/srfi-60.html) for R7RS-compliant Schemes.

## Running

For development I use [Chibi Scheme](//code.google.com/p/chibi-scheme) (the latest build from Mercurial trunk).

## Rationale

Chibi has support for [SRFI 33 'Integer Bitwise-operation Library'](//srfi.schemers.org/srfi-33/srfi-33.html),
but it is a withdrawn SRFI with no moral obligations to be portable. SRFI 60 is in _final_ state which is by
definition more stable and reliable.

(The real reason for this is that I want more personal experience with the C side of Chibi. Foreign interface
is cleanly separated from the portable code, and a fully-portable implementation is also provided, so other
R7RS implementations can also use this library.)

## Licensing

This SRFI implementation is distributed under **[3-clause BSD license](LICENSE)**.