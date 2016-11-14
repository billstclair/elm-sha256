# Sha256 Example

The file`example.elm` contains an example comparing billstclair/elm-sha256, spisemisu/elm-sha, and the native JS. To run it:

```
cd .../elm-sha256/example
elm-reactor
```

Then aim your browser at [localhost:8000/example.elm](http://localhost:8000/example.elm).

It takes about a second before the screen appears (on my 2011 iMac), while it runs Sha256.sha256, SHA.sha256sum, and the native JS code, 1,000 times each, to time them.

## Performance

The ```example``` directory contains the native JavaScript library, wrapped as an Elm package. If you include ```NativeSha256.elm``` and the ```Native``` directory in your project, you can do the following to run the JavaScript code:

```
import NativeSha256.elm

hash = NativeSha256.sha256("foo")
hash2 = NativeSha256.sha224("foo")
```

The reason you might _want_ to use the JS library is that it's over 30 times as fast as the Elm version. Maybe some day I'll work at speeding it up, but I'll bet the compiler team will help with that.

