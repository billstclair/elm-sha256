----------------------------------------------------------------------
--
-- Sha256.elm
-- sha256 & sha224 for Elm
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Sha256 exposing (sha256, sha224)

{-| This module is a Pure Elm implementation of the sha256 and sha224
crytographic hash functions.

Thank you to Yi-Cyuan Chen for the JavaScript I converted.


# Functions

@docs sha256, sha224

-}

import Array exposing (Array)
import Bitwise
    exposing
        ( and
        , or
        , shiftLeftBy
        , shiftRightBy
        , shiftRightZfBy
        )
import Char
import Debug exposing (log)
import List.Extra as LE
import String


sr : Int -> Int -> Int
sr num shift =
    shiftRightBy shift num


srz : Int -> Int -> Int
srz num shift =
    shiftRightZfBy shift num


sl : Int -> Int -> Int
sl num shift =
    shiftLeftBy shift num


lognot : Int -> Int
lognot =
    Bitwise.complement


extra : List Int
extra =
    [ -2147483648, 8388608, 32768, 128 ]


ks : Array Int
ks =
    Array.fromList
        [ 0x428A2F98
        , 0x71374491
        , 0xB5C0FBCF
        , 0xE9B5DBA5
        , 0x3956C25B
        , 0x59F111F1
        , 0x923F82A4
        , 0xAB1C5ED5
        , 0xD807AA98
        , 0x12835B01
        , 0x243185BE
        , 0x550C7DC3
        , 0x72BE5D74
        , 0x80DEB1FE
        , 0x9BDC06A7
        , 0xC19BF174
        , 0xE49B69C1
        , 0xEFBE4786
        , 0x0FC19DC6
        , 0x240CA1CC
        , 0x2DE92C6F
        , 0x4A7484AA
        , 0x5CB0A9DC
        , 0x76F988DA
        , 0x983E5152
        , 0xA831C66D
        , 0xB00327C8
        , 0xBF597FC7
        , 0xC6E00BF3
        , 0xD5A79147
        , 0x06CA6351
        , 0x14292967
        , 0x27B70A85
        , 0x2E1B2138
        , 0x4D2C6DFC
        , 0x53380D13
        , 0x650A7354
        , 0x766A0ABB
        , 0x81C2C92E
        , 0x92722C85
        , 0xA2BFE8A1
        , 0xA81A664B
        , 0xC24B8B70
        , 0xC76C51A3
        , 0xD192E819
        , 0xD6990624
        , 0xF40E3585
        , 0x106AA070
        , 0x19A4C116
        , 0x1E376C08
        , 0x2748774C
        , 0x34B0BCB5
        , 0x391C0CB3
        , 0x4ED8AA4A
        , 0x5B9CCA4F
        , 0x682E6FF3
        , 0x748F82EE
        , 0x78A5636F
        , 0x84C87814
        , 0x8CC70208
        , 0x90BEFFFA
        , 0xA4506CEB
        , 0xBEF9A3F7
        , 0xC67178F2
        ]


get : Int -> Array Int -> Int
get index array =
    -- The default is actually an error, but I don't expect it to happen.
    Maybe.withDefault 0 (Array.get index array)


orIntoBlocks : Int -> Int -> Blocks -> Blocks
orIntoBlocks idx val blocks =
    Array.set idx (or val (get idx blocks)) blocks


getAt : Int -> List Int -> Int
getAt index list =
    Maybe.withDefault 0 (LE.getAt index list)



-- Done with SHIFT array in JavaScript code
-- SHIFT = [24, 16, 8, 0]


getShift : Int -> Int -> Int
getShift i n =
    8 * (3 - and (i + n) 3)


type alias Message =
    Array Int


type alias Blocks =
    Array Int


indexLoop : Int -> Int -> Message -> Int -> Blocks -> ( Int, Int, Blocks )
indexLoop i index message length blocks =
    if not (index < length && i < 64) then
        ( i, index, blocks )

    else
        let
            code =
                get index message

            shift =
                getShift i

            ( iInc, idxInc, val ) =
                if code < 0x80 then
                    ( 1
                    , 0
                    , sl code (shift 0)
                    )

                else if code < 0x0800 then
                    ( 2
                    , 0
                    , sl (or 0xC0 (sr code 6)) (shift 0)
                        |> or (sl (or 0x80 (and code 0x3F)) (shift 1))
                    )

                else if code < 0xD800 || code >= 0xE000 then
                    ( 3
                    , 0
                    , sl (or 0xE0 (sr code 12)) (shift 0)
                        |> or
                            (sl (or 0x80 (and (sr code 6) 0x3F))
                                (shift 1)
                            )
                        |> or (sl (or 0x80 (and code 0x3F)) (shift 2))
                    )

                else
                    ( 4
                    , 1
                    , let
                        code2 =
                            0x00010000
                                + sl (and code 0x03FF) 10
                                |> or (and (get (index + 1) message) 0x03FF)
                      in
                      sl (or 0xF0 (shiftRightBy 18 code2)) (shift 0)
                        |> or
                            (sl (or 0x80 (and (shiftRightBy 12 code2) 0x3F))
                                (shift 1)
                            )
                        |> or
                            (sl (or 0x80 (and (sr code2 6) 0x3F))
                                (shift 2)
                            )
                        |> or
                            (sl (or 0x80 (and code2 0x3F))
                                (shift 3)
                            )
                    )

            blocks2 =
                orIntoBlocks (sr i 2) val blocks
        in
        indexLoop (i + iInc) (index + idxInc + 1) message length blocks2



-- Since I have the HS safely stored in a record,
-- I've dispensed with the names h0, h1, ..., h7
-- and just used a, b, ..., g throughout.


type alias HS =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    , e : Int
    , f : Int
    , g : Int
    , h : Int
    }


makeBlocks : Int -> Blocks
makeBlocks block =
    Array.set 0 block (Array.repeat 64 0)


jLoop1 : Int -> Blocks -> Blocks
jLoop1 j blocks =
    let
        t1 =
            get (j - 15) blocks

        t2 =
            get (j - 2) blocks

        s0 =
            or (srz t1 7) (sl t1 25)
                |> Bitwise.xor (or (srz t1 18) (sl t1 14))
                |> Bitwise.xor (srz t1 3)

        s1 =
            or (srz t2 17) (sl t2 15)
                |> Bitwise.xor (or (srz t2 19) (sl t2 13))
                |> Bitwise.xor (srz t2 10)

        blocks2 =
            Array.set
                j
                (sl
                    (get (j - 16) blocks
                        + s0
                        + get (j - 7) blocks
                        + s1
                    )
                    0
                )
                blocks
    in
    if j < 63 then
        jLoop1 (j + 1) blocks2

    else
        blocks2


jLoopBody2 : Int -> Int -> HS -> Blocks -> HS
jLoopBody2 j ab hs blocks =
    let
        a =
            hs.a

        b =
            hs.b

        c =
            hs.c

        d =
            hs.d

        e =
            hs.e

        f =
            hs.f

        g =
            hs.g

        h =
            hs.h

        s0 =
            or (srz d 2) (sl d 30)
                |> Bitwise.xor (or (srz d 13) (sl d 19))
                |> Bitwise.xor (or (srz d 22) (sl d 10))

        s1 =
            or (srz h 6) (sl h 26)
                |> Bitwise.xor (or (srz h 11) (sl h 21))
                |> Bitwise.xor (or (srz h 25) (sl h 7))

        da =
            and d a

        maj =
            Bitwise.xor da (and d b)
                |> Bitwise.xor ab

        ch =
            and h e
                |> Bitwise.xor (and (lognot h) f)

        t1 =
            g + s1 + ch + get (j + 1) ks + get (j + 1) blocks

        t2 =
            s0 + maj

        g2 =
            sl (c + t1) 0

        c2 =
            sl (t1 + t2) 0

        s2 =
            or (srz c2 2) (sl c2 30)
                |> Bitwise.xor (or (srz c2 13) (sl c2 19))
                |> Bitwise.xor (or (srz c2 22) (sl c2 10))

        s3 =
            or (srz g2 6) (sl g2 26)
                |> Bitwise.xor (or (srz g2 11) (sl g2 21))
                |> Bitwise.xor (or (srz g2 25) (sl g2 7))

        cd =
            and c2 d

        maj2 =
            Bitwise.xor cd (and c2 a)
                |> Bitwise.xor da

        ch2 =
            Bitwise.xor (and g2 h) (and (lognot g2) e)

        t3 =
            f + s3 + ch2 + get (j + 2) ks + get (j + 2) blocks

        t4 =
            s2 + maj2

        f2 =
            sl (b + t3) 0

        b2 =
            sl (t3 + t4) 0

        s4 =
            or (srz b2 2) (sl b2 30)
                |> Bitwise.xor (or (srz b2 13) (sl b2 19))
                |> Bitwise.xor (or (srz b2 22) (sl b2 10))

        s5 =
            or (srz f2 6) (sl f2 26)
                |> Bitwise.xor (or (srz f2 11) (sl f2 21))
                |> Bitwise.xor (or (srz f2 25) (sl f2 7))

        bc =
            and b2 c2

        maj3 =
            Bitwise.xor bc (and b2 d)
                |> Bitwise.xor cd

        ch3 =
            Bitwise.xor (and f2 g2) (and (lognot f2) h)

        t5 =
            e + s5 + ch3 + get (j + 3) ks + get (j + 3) blocks

        t6 =
            s4 + maj3

        e2 =
            sl (a + t5) 0

        a2 =
            sl (t5 + t6) 0
    in
    { a = a2
    , b = b2
    , c = c2
    , d = d
    , e = e2
    , f = f2
    , g = g2
    , h = h
    }


jLoop2 : Int -> Bool -> Bool -> HS -> Blocks -> HS
jLoop2 j first is224 hs blocks =
    let
        ( ab, h, d ) =
            if first then
                if is224 then
                    let
                        t1 =
                            get 0 blocks - 1413257819
                    in
                    ( 300032
                      --ab
                    , sl (t1 - 150054599) 0
                      --h
                    , sl (t1 + 24177077) 0
                      --d
                    )

                else
                    let
                        t2 =
                            get 0 blocks - 210244248
                    in
                    ( 704751109
                      --ab
                    , sl (t2 - 1521486534) 0
                      --h
                    , sl (t2 + 143694565) 0
                    )
                --d

            else
                let
                    s0 =
                        or (srz hs.a 2) (sl hs.a 30)
                            |> Bitwise.xor (or (srz hs.a 13) (sl hs.a 19))
                            |> Bitwise.xor (or (srz hs.a 22) (sl hs.a 10))

                    s1 =
                        or (srz hs.e 6) (sl hs.e 26)
                            |> Bitwise.xor (or (srz hs.e 11) (sl hs.e 21))
                            |> Bitwise.xor (or (srz hs.e 25) (sl hs.e 7))

                    ab2 =
                        and hs.a hs.b

                    maj =
                        Bitwise.xor ab2 (and hs.a hs.c)
                            |> Bitwise.xor (and hs.b hs.c)

                    ch =
                        Bitwise.xor (and hs.e hs.f) (and (lognot hs.e) hs.g)

                    t3 =
                        hs.h + s1 + ch + get j ks + get j blocks

                    t4 =
                        s0 + maj
                in
                ( ab2
                  --ab
                , sl (hs.d + t3) 0
                  --h
                , sl (t3 + t4) 0
                  --d
                )

        hs2 =
            { hs | h = h, d = d }

        hs3 =
            jLoopBody2 j ab hs2 blocks

        first2 =
            False

        jp4 =
            j + 4
    in
    if jp4 < 64 then
        jLoop2 jp4 first2 is224 hs3 blocks

    else
        hs3


sumHS : HS -> HS -> HS
sumHS hs1 hs2 =
    { a = sl (hs1.a + hs2.a) 0
    , b = sl (hs1.b + hs2.b) 0
    , c = sl (hs1.c + hs2.c) 0
    , d = sl (hs1.d + hs2.d) 0
    , e = sl (hs1.e + hs2.e) 0
    , f = sl (hs1.f + hs2.f) 0
    , g = sl (hs1.g + hs2.g) 0
    , h = sl (hs1.h + hs2.h) 0
    }


outerLoop : Bool -> HS -> Int -> Int -> Int -> Int -> Bool -> Message -> Int -> HS
outerLoop first hs block start bytes index is224 message length =
    let
        blocks =
            makeBlocks block

        ( i, index2, blocks2 ) =
            indexLoop start index message length blocks

        bytes2 =
            bytes + i - start

        start2 =
            i - 64

        ( blocks3, index3 ) =
            if index2 == length then
                ( orIntoBlocks (sr i 2) (getAt (and i 3) extra) blocks2
                , index2 + 1
                )

            else
                ( blocks2, index2 )

        block2 =
            get 16 blocks3

        ( end, blocks4 ) =
            if index3 > length && i < 56 then
                ( True, Array.set 15 (sl bytes2 3) blocks3 )

            else
                ( False, blocks3 )

        blocks5 =
            jLoop1 16 blocks4

        hs2 =
            jLoop2 0 first is224 hs blocks5

        hs3 =
            sumHS hs hs2
    in
    if not end then
        outerLoop False hs3 block2 start2 bytes2 index3 is224 message length

    else
        hs3



-- Convert the low 4 bits of a number to a hex character.


toHex1 : Int -> Char
toHex1 x =
    let
        x2 =
            and x 0x0F
    in
    Char.fromCode
        (x2
            + (if x2 < 10 then
                Char.toCode '0'

               else
                -10 + Char.toCode 'a'
              )
        )



-- Convert the low 32 bits of an integer to a 4-character hex string.


toHex8 : Int -> String
toHex8 x =
    String.fromList
        (List.map (\shift -> toHex1 (sr x shift))
            [ 28, 24, 20, 16, 12, 8, 4, 0 ]
        )



-- Convert 7 elements of an HS instance to a 56-character hex string.


toHex56 : HS -> String
toHex56 hs =
    List.foldr
        (++)
        ""
        (List.map
            toHex8
            [ hs.a, hs.b, hs.c, hs.d, hs.e, hs.f, hs.g ]
        )



-- Convert 8 elements of an HS instance to a 64-character hex string.


toHex64 : HS -> String
toHex64 hs =
    toHex56 hs ++ toHex8 hs.h


stringToMessage : String -> Message
stringToMessage string =
    Array.fromList (List.map Char.toCode (String.toList string))


initialHs : Bool -> HS
initialHs is224 =
    if is224 then
        -- sha224
        { a = 0xC1059ED8
        , b = 0x367CD507
        , c = 0x3070DD17
        , d = 0xF70E5939
        , e = 0xFFC00B31
        , f = 0x68581511
        , g = 0x64F98FA7
        , h = 0xBEFA4FA4
        }

    else
        -- sha256
        { a = 0x6A09E667
        , b = 0xBB67AE85
        , c = 0x3C6EF372
        , d = 0xA54FF53A
        , e = 0x510E527F
        , f = 0x9B05688C
        , g = 0x1F83D9AB
        , h = 0x5BE0CD19
        }


hash : String -> Bool -> String
hash string is224 =
    let
        hs =
            initialHs is224

        block =
            0

        start =
            0

        bytes =
            0

        index =
            0

        message =
            stringToMessage string

        length =
            Array.length message

        hs2 =
            outerLoop True hs block start bytes index is224 message length
    in
    if is224 then
        toHex56 hs2

    else
        toHex64 hs2


{-| Returns the sha256 hash of its argument.
-}
sha256 : String -> String
sha256 string =
    hash string False


{-| Returns the sha224 hash of its argument.
-}
sha224 : String -> String
sha224 string =
    hash string True
