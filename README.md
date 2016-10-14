# sha256 and sha224 Cryptographically Secure Hash Functions for Elm

A mindless conversion of a JavaScript sha256 library to Elm.

Thank you to Yi-Cyuan Chen for the JavaScript ([github.com/emn178/js-sha256](https://github.com/emn178/js-sha256)).

It's as simple as:

```
import Sha256 exposing (sha256, ssa224)

hash = sha256("foo")
hash2 = sha224("foo")
```

`example/example.elm` contains an example comparing the pure Elm code to a native module. To run it:

```
cd .../elm-sha256/example
elm-reactor
```

Then aim your browser at [localhost:8000/example.elm](http://localhost:8000/example.elm).

## Performance

The source contains the native JavaScript library, wrapped as an Elm package. You should be able to see from the example how to use it.

The reason you might _want_ to use the JS library is that it's 40 times as fast as the Elm version. The code is unoptimized. Just a fast-and-dirty translation. There are probably some tricks that could make it quite a bit faster without a lot of work, and I may try at some time, but for now, it works.

## Tests

There are some very simple tests, running the hash functions on the same three strings as are displayed in the example. To run them, you'll need ```Node.js``` and ```make```:

```
cd .../elm-sha256/test
make
```
