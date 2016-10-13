import PureSha256 exposing (sha256, sha224)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import String
import Char

strings : List String
strings =
  [ "foo"
  , "The quick brown fox jumped over the lazy dog."
  , "Four score and seven years ago..."
  ]

stringFromCode : Int -> String
stringFromCode code =
  String.fromList [(Char.fromCode code)]

nbsp : String
nbsp = stringFromCode 160   -- \u00A0

br : Html a
br = Html.br [] []

render : String -> String -> String -> Html a
render string hashName hash =
  div [ style [ ("font-family", "'Courier New', Courier, monospace") ] ]
    [ text (hashName ++ "(\"" ++ string ++ "\") = ")
    , br
    , text (nbsp ++ nbsp ++ "\"" ++ hash ++ "\"")
    , br
    ]

render256 : String -> Html a
render256 string =
  -- ** Don't blink, here's the sha256 call. **
  render string "sha256" (sha256 string)

render224 : String -> Html a
render224 string =
  -- ** Don't blink, here's the sha224 call. **
  render string "sha224" (sha224 string)

main =
  div []
    [ p []
        (List.map render256 strings)
    , p []
        (List.map render224 strings)
    ]
