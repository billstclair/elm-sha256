----------------------------------------------------------------------
--
-- example.elm
-- billstclair/elm-sha256 package example
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

import Sha256 exposing (sha256, sha224)
import NativeSha256 as N

import String
import Char
import Time exposing (Time)
import Task

import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (style)
import Html.App as Html

import Formatting exposing (print, (<>), s, string, roundTo, int)

--
-- Here are the calls of the two exposed Sha256 functions.
-- The rest of this file is GUI (and timing).
--

render256 : String -> Html a
render256 string =
  render string "sha256" (sha256 string) (N.sha256 string)

render224 : String -> Html a
render224 string =
  render string "sha224" (sha224 string) (N.sha224 string)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

timingIterations : Int
timingIterations = 1000

type alias Model =
  { startTime : Maybe Time
  , elmTime : Maybe Float
  , nativeTime: Maybe Float
  }
      
timeCmd : Cmd Msg
timeCmd =
  Task.perform (\x -> Nop) (\x -> Now x) Time.now

init : (Model, Cmd Msg)
init = (model, timeCmd)

model : Model
model =
  Model
    Nothing -- startTime
    Nothing -- elmTime
    Nothing -- nativeTime

-- UPDATE

type Msg
  = Now Time
  | Nop

timingMessage : String
timingMessage =
  "The quick brown fox jumped onto a magic carpet with the lazy god."

timeIt : Int -> (String -> String) -> Int
timeIt iterations hasher =
  let loop = (\cnt ->
                if cnt >= iterations then
                  cnt
                else
                  let _ = hasher timingMessage
                  in
                      loop (cnt + 1))
  in
      loop 0

timeElm : Int -> Int
timeElm iterations =
  timeIt iterations sha256

timeNative : Int -> Int
timeNative iterations =
  timeIt iterations N.sha256

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Nop ->
        ( model, Cmd.none )
      Now time ->
        case model.startTime of
            Nothing ->
              let _ = timeElm timingIterations
              in
                  ( { model | startTime = Just time }
                  , timeCmd
                  )
            Just startTime ->
              case model.elmTime of
                  Nothing ->
                    let _ = timeNative timingIterations
                    in
                        ( { model |
                             startTime = Just time
                           , elmTime = Just (time - startTime)
                          }
                        , timeCmd
                        )
                  Just _ ->
                    ( { model | nativeTime = Just (time - startTime) }
                    , Cmd.none
                    )
              
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

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

copyright: String
copyright = stringFromCode 169  -- \u00A9

br : Html a
br = Html.br [] []

hashFormat = string <> s "(\"" <> string <> s "\")"

formatHash : String -> String -> String
formatHash name string =
  print hashFormat name string

resultFormat = string <> string <> s ": \"" <> string <> s "\""

formatResult : Int -> String -> String -> String
formatResult spaces kind hash =
  print resultFormat (String.repeat spaces nbsp) kind hash

render : String -> String -> String -> String -> Html a
render string hashName hash nativeHash =
  div []
    [ text <| formatHash hashName string
    , br
    , text <| formatResult 2 "Elm" hash
    , br
    , text <| formatResult 4 "JS" nativeHash
    ]
    
timeFormat =
  string <> s " time: " <> roundTo 1 <> s " microseconds / hash"

formatTime : String -> Float -> String
formatTime kind time =
  let millis = time * Time.millisecond * 1000 / toFloat timingIterations
  in
      print timeFormat kind millis

ratioFormat =
  s "Elm/JS = " <> roundTo 2

formatRatio : Float -> Float -> String
formatRatio elmTime nativeTime =
  print ratioFormat <| elmTime / nativeTime

copyrightFormat =
  s "Copyright " <> s copyright <> s " " <>
    int <> s " " <> string <> s " <" <> string <> s ">"

formatCopyright : Int -> String -> String -> String
formatCopyright year who email =
  print copyrightFormat year who email

renderScreen : Float -> Float -> Html Msg
renderScreen elmTime nativeTime =
  div [ style [ ("font-family", "Arial, Helvetica, sans-serif")
              , ("font-size", "125%")
              ]
      ]
    [ h2 []
        [ text "elm-sha256 Demo" ]
    , p []
        [ text <| formatTime "Elm" elmTime
        , br
        , text <| formatTime "JS" nativeTime
        ,br
        , text <| formatRatio elmTime nativeTime
        ]
    , p []
       (List.map render256 strings)
    , p []
       (List.map render224 strings)
    , p [ style [ ("font-size", "75%") ] ]
       [ text <| formatCopyright 2016 "Bill St. Clair" "billstclair@gmail.com" ]
    ]

view : Model -> Html Msg
view model =
  case model.elmTime of
      Nothing ->
        text "Timing Elm..."
      Just elmTime ->
        case model.nativeTime of
            Nothing ->
              text "Timing JS..."
            Just nativeTime ->
              renderScreen elmTime nativeTime
