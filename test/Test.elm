module Test exposing (main)

{-| The main entry point for the tests.

@docs main
-}

import ElmTest exposing (Test, suite, runSuite)
import Sha256Tests


tests : Test
tests =
    suite "All"
        [ Sha256Tests.tests ]


{-| Run the test suite under node.
-}
main : Program Never
main =
    runSuite tests
