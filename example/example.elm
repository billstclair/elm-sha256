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
-- The SHA timings are commented out until spisemisu/elm-sha is upgraded to 0.18


module Main exposing (..)

import Sha256 exposing (sha256, sha224)
import NativeSha256 as N


-- import SHA

import String
import Char
import Time exposing (Time)
import Task
import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (style)
import Formatting exposing (print, (<>), s, string, roundTo, int)


--
-- Here are the calls of the two exposed Sha256 functions.
-- The rest of this file is GUI (and timing).
--


render256 : String -> Html a
render256 string =
    render string "sha256" (sha256 string) (N.sha256 string)



--(SHA.sha256sum string)


render224 : String -> Html a
render224 string =
    render string "sha224" (sha224 string) (N.sha224 string)



--(SHA.sha224sum string)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


timingIterations : Int
timingIterations =
    1000


type alias Model =
    { startTime : Maybe Time
    , elmTime : Maybe Float
    , nativeTime : Maybe Float
    , shaTime : Maybe Float
    }


timeCmd : Cmd Msg
timeCmd =
    Task.perform (\x -> Now x) Time.now


init : ( Model, Cmd Msg )
init =
    ( model, timeCmd )


model : Model
model =
    Model
        Nothing
        -- startTime
        Nothing
        -- elmTime
        Nothing
        -- nativeTime
        Nothing



-- shaTime
-- UPDATE


type Msg
    = Now Time
    | Nop


timingMessage : String
timingMessage =
    "The quick brown fox jumped onto a magic carpet with the lazy god."


timeIt : Int -> (String -> String) -> Int
timeIt iterations hasher =
    let
        loop =
            (\cnt ->
                if cnt >= iterations then
                    cnt
                else
                    let
                        _ =
                            hasher timingMessage
                    in
                        loop (cnt + 1)
            )
    in
        loop 0


timeElm : Int -> Int
timeElm iterations =
    timeIt iterations sha256


timeNative : Int -> Int
timeNative iterations =
    timeIt iterations N.sha256



{-
   timeSHA : Int -> Int
   timeSHA iterations =
       timeIt iterations SHA.sha256sum
-}


maybeTime :
    Time
    -> Model
    -> List ( Maybe Time, Int -> Int, Time -> Model -> Model )
    -> ( Model, Cmd Msg )
maybeTime now model specs =
    case specs of
        [] ->
            ( model, Cmd.none )

        head :: tail ->
            let
                ( time, tester, updater ) =
                    head
            in
                case time of
                    Nothing ->
                        let
                            _ =
                                tester timingIterations

                            startTime =
                                case model.startTime of
                                    Nothing ->
                                        0

                                    Just t ->
                                        t
                        in
                            ( updater
                                (now - startTime)
                                { model | startTime = Just now }
                            , timeCmd
                            )

                    Just _ ->
                        maybeTime now model tail


updateNothing : Float -> Model -> Model
updateNothing time model =
    model


updateElmTime : Float -> Model -> Model
updateElmTime time model =
    { model | elmTime = Just time }


updateNativeTime : Float -> Model -> Model
updateNativeTime time model =
    { model | nativeTime = Just time }



{-
   updateSHATime : Float -> Model -> Model
   updateSHATime time model =
       { model | shaTime = Just time }
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Now time ->
            maybeTime
                time
                model
                [ ( model.startTime, timeNative, updateNothing )
                , ( model.nativeTime, timeElm, updateNativeTime )
                , ( model.elmTime, identity, updateElmTime )
                  --, ( model.shaTime, identity, updateSHATime )
                ]



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
    String.fromList [ (Char.fromCode code) ]


nbsp : String
nbsp =
    stringFromCode 160



-- \u00A0


copyright : String
copyright =
    stringFromCode 169



-- \u00A9


br : Html a
br =
    Html.br [] []


hashFormat =
    string <> s "(\"" <> string <> s "\")"


formatHash : String -> String -> String
formatHash name string =
    print hashFormat name string


resultFormat =
    string <> string <> s ": \"" <> string <> s "\""


formatResult : Int -> String -> String -> String
formatResult spaces kind hash =
    print resultFormat (String.repeat spaces nbsp) kind hash


render : String -> String -> String -> String {- -> String -} -> Html a
render string hashName hash nativeHash =
    --shaHash =
    div []
        [ text <| formatHash hashName string
        , br
        , text <| formatResult 2 "Sha256" hash
          --, br
          --, text <| formatResult 7 "SHA" shaHash
        , br
        , text <| formatResult 10 "JS" nativeHash
        ]


timeFormat =
    string <> s " time: " <> roundTo 1 <> s " microseconds / hash"


formatTime : String -> Float -> String
formatTime kind time =
    let
        millis =
            time * Time.millisecond * 1000 / toFloat timingIterations
    in
        print timeFormat kind millis


ratioFormat =
    string <> s " = " <> roundTo 2


formatRatio : String -> Float -> Float -> String
formatRatio label otherTime nativeTime =
    print ratioFormat label (otherTime / nativeTime)


copyrightFormat =
    s "Copyright "
        <> s copyright
        <> s " "
        <> int
        <> s " "
        <> string
        <> s " <"
        <> string
        <> s ">"


formatCopyright : Int -> String -> String -> String
formatCopyright year who email =
    print copyrightFormat year who email


renderScreen : Float -> Float {- -> Float -} -> Html Msg
renderScreen elmTime nativeTime =
    --shaTime =
    div
        [ style
            [ ( "font-family", "Arial, Helvetica, sans-serif" )
            , ( "font-size", "125%" )
            ]
        ]
        [ h2 []
            [ text "elm-sha256 Demo" ]
        , p []
            [ text <| formatTime "Sha256" elmTime
              --, br
              --, text <| formatTime "SHA" shaTime
            , br
            , text <| formatTime "JS" nativeTime
            , br
            , text <| formatRatio "Sha256/JS" elmTime nativeTime
              --, br
              --, text <| formatRatio "SHA/JS" shaTime nativeTime
            ]
        , p []
            (List.map render256 strings)
        , p []
            (List.map render224 strings)
        , p [ style [ ( "font-size", "75%" ) ] ]
            [ text <| formatCopyright 2016 "Bill St. Clair" "billstclair@gmail.com" ]
        ]


view : Model -> Html Msg
view model =
    case ( model.elmTime, model.nativeTime ) of
        --, model.shaTime ) of
        ( Just elmTime, Just nativeTime ) ->
            --, Just shaTime ) ->
            renderScreen elmTime nativeTime

        -- shaTime
        a ->
            text "Timing..."
