# sha256 and sha224 Cryptographically Secure Hash Functions for Elm

A simple conversion of a JavaScript sha256 library to Elm.

Thank you to Yi-Cyuan Chen for the JavaScript ([github.com/emn178/js-sha256](https://github.com/emn178/js-sha256)).

To install it in your development directory:

```
elm package install billstclair/elm-sha256
```

To use it from your code:

```
import Sha256 exposing (sha256, sha224)

hash = sha256("foo")
hash2 = sha224("foo")
```
