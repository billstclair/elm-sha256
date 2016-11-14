module Sha256Tests exposing (tests)

import ElmTest exposing (..)
import Sha256 exposing (sha256, sha224)


tests : Test
tests =
    ElmTest.suite "sha256"
        [ basicTests
        ]


data : List ( String, String, String )
data =
    [ ( "foo"
      , "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
      , "0808f64e60d58979fcb676c96ec938270dea42445aeefcd3a4e6f8db"
      )
    , ( "The quick brown fox jumped over the lazy dog."
      , "68b1282b91de2c054c36629cb8dd447f12f096d3e3c587978dc2248444633483"
      , "0c38eb6c6e803f9fbcd06f006506bde86a7dd84a1b5f098d43d45e56"
      )
    , ( "Four score and seven years ago..."
      , "0d8e421d91346d5680433b294b2ac4cfba46cc8dfb36272af7994216aa781dfb"
      , "4e29b711e9cc0bc8510b43cc61f3bc27f17c7f1a4cf0aab9817937fd"
      )
    ]


doDefaultTests : ( String, String, String ) -> List Test
doDefaultTests ( message, expected256, expected224 ) =
    List.map defaultTest
        [ assertEqual expected256 <|
            sha256 message
        , assertEqual expected224 <|
            sha224 message
        ]


basicTests : Test
basicTests =
    ElmTest.suite "Basic" <|
        List.concatMap doDefaultTests data
