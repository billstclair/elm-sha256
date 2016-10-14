# sha256 and sha224 Cryptographically Secure Hash Functions for Elm

A mindless conversion of a JavaScript sha256 library to Elm.

Thank you to Yi-Cyuan Chen for the JavaScript ([github.com/emn178/js-sha256](https://github.com/emn178/js-sha256)).

To install it in your development directory:

```
elm package install billstclair/elm-sha256
```

To use it from your code:

```
import Sha256 exposing (sha256, ssa224)

hash = sha256("foo")
hash2 = sha224("foo")
```

`example/example.elm` contains an example comparing billstclair/elm-sha256, spisemisu/elm-sha, and the native JS. To run it:

```
cd .../elm-sha256/example
elm-reactor
```

Then aim your browser at [localhost:8000/example.elm](http://localhost:8000/example.elm).

It takes about a second before the screen appears (on my 2011 iMac), while it runs Sha256.sha256, SHA.sha256sum, and the native JS code, 1,000 times each, to time them.

## Performance

The example directory contains the native JavaScript library, wrapped as an Elm package. You should be able to see from there how to use it.

The reason you might _want_ to use the JS library is that it's 40 times as fast as the Elm version. The code is unoptimized. Just a fast-and-dirty translation. There are probably some tricks that could make it quite a bit faster without a lot of work, and I may try at some time, but for now, it works.

## Tests

There are some very simple tests, running the hash functions on the same three strings as are displayed in the example. To run them, you'll need ```Node.js``` and ```make```:

```
cd .../elm-sha256/test
make
```
