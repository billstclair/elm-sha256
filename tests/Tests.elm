module Tests exposing (all)

import Test exposing (..)
import Expect
import List

import Sha256 exposing (sha256, sha224)

all : Test
all =
    Test.concat <|
        List.concatMap shaTests data

shaTests : ( String, String, String ) -> List Test
shaTests ( message, expected256, expected224 ) =
    [ shaTest message "sha256" expected256 sha256
    , shaTest message "sha224" expected224 sha224
    ]
    
shaTest : String -> String -> String -> (String -> String) -> Test
shaTest message hashName expected hasher =
    test (hashName ++ " of \"" ++ message ++ "\"") <|
        \() ->
            let res = hasher message
            in
              Expect.equal res expected

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
    , ( "Four score and\nseven years ago..."
      , "8f4af3499507bb185b21adcacf8d5be07f1f38e77a50c8a789f8d38d39818be2"
      , "89a28e6540c7d1ca3cd88902394a3c96c9babd31d1e1103fc00f7692"
      )
    , ( ""
      , "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      , "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f"
      )
    , ( "=&-:;/"
      , "67ae4ac1f04f03303a79394c43502e490f354f0339ed3bda73ae342cf908b0a1"
      , "569a9dc6c54b57d669f63423f4c0506af9bc54fcb9094936395b2093"
      )
    , ( ".........1.........2.........3.........4.........512345"
      , "4a1805f759bfb081ab50d803a8551211cbcdb2fb01f62f64368f052cf7810c4d"
      , "8bb477e85920defbb925ed1ad10f1c6c8fbd21cc7f7178d04ab39346"
      )
    , ( "the quick brown fox jumps over the big lazy purple hand"
      , "b57ccd08359cfc8001edb6a6f241d6888b214815654937f42440e0d5cb3de88c"
      , "80233ae81503b76434439a75318dad9eb4874ede3c633a02d13da10c"
      )
    , ( ".........1.........2.........3.........4.........5123456"
      , "297dddace1ef27fe2dd4ed86e2fe411afe2fb57c9e7f2e98e1e9e90f4f8e55ec"
      , "8d97d22259ecbd444f3fd94fa2146601b85574f1b7ac9f3f9c5da984"
      )
    , ( "the quick brown fox jumps over the big lazy purple hands"
      , "b782b0c1557537ef9db3e478e90fcffe6bd528f0e24f7283f1db7eaa9f22d390"
      , "197fee48f85d7dc1bea29f695c4572bb3dd68fa132cc24402f2d4b5c"
      )
    , ( """GET
      /
      Action=ListUsers&Version=2010-05-08
      content-type:application/x-www-form-urlencoded; charset=utf-8
      host:iam.amazonaws.com
      x-amz-date:20150830T123600Z

      content-type;host;x-amz-date
      e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"""
      , "f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59"
      , "11c62d9e7a542409ef5e5b4686de673cab0df7fe715f65a61cf8c929"
      )
    ]
