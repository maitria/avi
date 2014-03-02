# avi

A lively vi.

## Vision

We love vim. We want more! Test coverage. Flexibilty. Live REPLs! 

## Guiding Principles

* Test driven. All functionality covered by tests.
* Don't defeat vim muscle memory.
* Code is for people. Be expressive as hell.
* Be friendly. Especially to noobs.

## Status

Our intention is for the unit tests to provide friendly documention of what's been implemented so far.
To run the unit tests with verbose output, use

```
$ lein midje :print-facts
```

## Installing

Works with Leiningen 2.3.

```
$ git clone https://github.com/maitria/avi.git
$ cd avi
$ lein install
```

## License

Copyright 2014 Maitria

You have permission to use this in any way you like (modify it, sell it, republish it), 
provided you agree to all the following conditions:

* you don't mislead anyone about it
* you don't interfere with our ability to use it
* you release us from any claims of liability if it causes problems for you
