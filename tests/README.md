## Tests

This directory contains a very simple test suite, running the hash functions on the same three strings as are displayed in the example. To run them, you'll need [Node.js](https://nodejs.org/en/). To install `elm-test`:

```
npm install -g elm-test
```

To run the SHA256 tests:

```
cd .../elm-sha256    # NOT the tests sub-directory
elm-test
```

To see results of individual tests (there are only 6 of them):

```
elm-test --report json
```
