# sha256 and sha224 Cryptographically Secure Hash Functions for Elm

A mindless conversion of a JavaScript sha256 library to Elm.

Thank you to Yi-Cyuan Chen for the JavaScript ([github.com/emn178/js-sha256](https://github.com/emn178/js-sha256)).

It's as simple as:

```
import Sha256 exposing (sha256)

hash = sha256("foo")
```

`example/example.elm` contains an example comparing the pure Elm code to a native module. To run it:

```
cd .../elm-sha256/example
elm-reactor
```

Then aim your browser at [localhost:8000/example.elm](http://localhost:8000/example.elm).
