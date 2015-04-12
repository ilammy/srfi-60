# SRFI 60 'Integers as Bits' [![Build Status](https://travis-ci.org/ilammy/srfi-60.svg?branch=master)](https://travis-ci.org/ilammy/srfi-60)

This is an implementation of [SRFI 60 'Integers as Bits'](//srfi.schemers.org/srfi-60/srfi-60.html) for R7RS-compliant Schemes.

## Installation

I use [Chibi Scheme](//github.com/ashinn/chibi-scheme) for development (the latest build from Git master).

You can run `./tools/make-snowball` script to package the library into a snowball which can be installed
then by Snow package manager.

## Rationale

Chibi has support for [SRFI 33 'Integer Bitwise-operation Library'](//srfi.schemers.org/srfi-33/srfi-33.html),
but it is a withdrawn SRFI with no moral obligations to be portable. SRFI 60 is in _final_ state which is by
definition more stable and reliable.

## Licensing

This SRFI implementation is distributed under **[3-clause BSD license](LICENSE)**.
