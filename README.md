# avi

https://github.com/maitria/avi

A lively vi.

## Vision

We love vim. We want more! Test coverage. Flexibilty. Live REPLs! 

## Guiding Principles

* Test driven. All functionality covered by tests.
* Don't defeat vim muscle memory.
* Code is for people. Be expressive as hell.
* Be friendly. Especially to noobs.

## Code of Welcoming Conduct

Everybody has boundaries around who they invite into their home. To help you decide whether you want to hang out with us, we've done our best to describe our boundaries here in the [maitria code of conduct](http://maitria.com/coc). It's not a bunch of legalese. It's who we are. Please read it.

## Contributing

We track upcoming work on a [Trello board].  This board has many small things
that are easy to pick up, and we'd love to see you.

I (Jason Felice) would love to walk through the code with you pretty much any
time during the US Eastern work day.  Ping me any time on Twitter - I'm
[@eraserhd] - or gchat.

[Trello board]: https://trello.com/b/E2LFvVLy/avi
[@eraserhd]: https://twitter.com/eraserhd

## Status

Our intention is for the unit tests to provide friendly documention of what's
been implemented so far.  To run the unit tests with verbose output, use

```
$ lein midje :print-facts
```

## Installing

* Works with Leiningen 2.3.4
* On Linux, `libncursesw5-dev` or equivalent must be installed.

```
$ git clone https://github.com/maitria/avi.git
$ cd avi
$ lein install /usr/local
```

On Linux, you'll likely not have access to `/usr/local`.  In that case you'll need to use
`sudo lein install` instead or specify a different installation directory.

## License

Copyright 2014 Maitria

You have permission to use this in any way you like (modify it, sell it, republish it), 
provided you agree to all the following conditions:

* you don't mislead anyone about it
* you don't interfere with our ability to use it
* you release us from any claims of liability if it causes problems for you
