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


module Sha256
    exposing
        ( sha256
        , sha224
        )

{-| This module is a Pure Elm implementation of the sha256 and sha224
crytographic hash functions.

Thank you to Yi-Cyuan Chen for the JavaScript I converted.

# Functions
@docs sha256, sha224
-}

import String
import Char
import List.Extra as LE
import Array exposing (Array)
import BitwiseInfix exposing (..)
import Debug exposing (log)


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
    Array.set idx (val ~| (get idx blocks)) blocks


getAt : Int -> List Int -> Int
getAt index list =
    Maybe.withDefault 0 (LE.getAt index list)



-- Done with SHIFT array in JavaScript code
-- SHIFT = [24, 16, 8, 0]


getShift : Int -> Int -> Int
getShift i n =
    8 * (3 - ((i + n) ~& 3))


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
                (get index message)

            shift =
                getShift i

            ( iInc, idxInc, val ) =
                if code < 0x80 then
                    ( 1
                    , 0
                    , code ~<< (shift 0)
                    )
                else if code < 0x0800 then
                    ( 2
                    , 0
                    , ((0xC0 ~| (code ~>> 6)) ~<< (shift 0))
                        ~| ((0x80 ~| (code ~& 0x3F)) ~<< (shift 1))
                    )
                else if code < 0xD800 || code >= 0xE000 then
                    ( 3
                    , 0
                    , ((0xE0 ~| (code ~>> 12)) ~<< (shift 0))
                        ~| ((0x80 ~| ((code ~>> 6) ~& 0x3F)) ~<< (shift 1))
                        ~| ((0x80 ~| (code ~& 0x3F)) ~<< (shift 2))
                    )
                else
                    ( 4
                    , 1
                    , let
                        code2 =
                            0x00010000
                                + (((code ~& 0x03FF) ~<< 10)
                                    ~| ((get (index + 1) message) ~& 0x03FF)
                                  )
                      in
                        ((0xF0 ~| (code2 ~>> 18)) ~<< (shift 0))
                            ~| ((0x80 ~| ((code2 ~>> 12) ~& 0x3F)) ~<< (shift 1))
                            ~| ((0x80 ~| ((code2 ~>> 6) ~& 0x3F)) ~<< (shift 2))
                            ~| ((0x80 ~| (code2 ~& 0x3F)) ~<< (shift 3))
                    )

            blocks2 =
                orIntoBlocks (i ~>> 2) val blocks
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
            ((t1 ~>>> 7) ~| (t1 ~<< 25))
                ~^ ((t1 ~>>> 18) ~| (t1 ~<< 14))
                ~^ (t1 ~>>> 3)

        s1 =
            ((t2 ~>>> 17) ~| (t2 ~<< 15))
                ~^ ((t2 ~>>> 19) ~| (t2 ~<< 13))
                ~^ (t2 ~>>> 10)

        blocks2 =
            Array.set
                j
                (((get (j - 16) blocks)
                    + s0
                    + (get (j - 7) blocks)
                    + s1
                 )
                    ~<< 0
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
            ((d ~>>> 2) ~| (d ~<< 30))
                ~^ ((d ~>>> 13) ~| (d ~<< 19))
                ~^ ((d ~>>> 22) ~| (d ~<< 10))

        s1 =
            ((h ~>>> 6) ~| (h ~<< 26))
                ~^ ((h ~>>> 11) ~| (h ~<< 21))
                ~^ ((h ~>>> 25) ~| (h ~<< 7))

        da =
            d ~& a

        maj =
            da ~^ (d ~& b) ~^ ab

        ch =
            (h ~& e) ~^ ((lognot h) ~& f)

        t1 =
            g + s1 + ch + (get (j + 1) ks) + (get (j + 1) blocks)

        t2 =
            s0 + maj

        g2 =
            c + t1 ~<< 0

        c2 =
            t1 + t2 ~<< 0

        s2 =
            ((c2 ~>>> 2) ~| (c2 ~<< 30))
                ~^ ((c2 ~>>> 13) ~| (c2 ~<< 19))
                ~^ ((c2 ~>>> 22) ~| (c2 ~<< 10))

        s3 =
            ((g2 ~>>> 6) ~| (g2 ~<< 26))
                ~^ ((g2 ~>>> 11) ~| (g2 ~<< 21))
                ~^ ((g2 ~>>> 25) ~| (g2 ~<< 7))

        cd =
            c2 ~& d

        maj2 =
            cd ~^ (c2 ~& a) ~^ da

        ch2 =
            (g2 ~& h) ~^ ((lognot g2) ~& e)

        t3 =
            f + s3 + ch2 + (get (j + 2) ks) + (get (j + 2) blocks)

        t4 =
            s2 + maj2

        f2 =
            b + t3 ~<< 0

        b2 =
            t3 + t4 ~<< 0

        s4 =
            ((b2 ~>>> 2) ~| (b2 ~<< 30))
                ~^ ((b2 ~>>> 13) ~| (b2 ~<< 19))
                ~^ ((b2 ~>>> 22) ~| (b2 ~<< 10))

        s5 =
            ((f2 ~>>> 6) ~| (f2 ~<< 26))
                ~^ ((f2 ~>>> 11) ~| (f2 ~<< 21))
                ~^ ((f2 ~>>> 25) ~| (f2 ~<< 7))

        bc =
            b2 ~& c2

        maj3 =
            bc ~^ (b2 ~& d) ~^ cd

        ch3 =
            (f2 ~& g2) ~^ ((lognot f2) ~& h)

        t5 =
            e + s5 + ch3 + (get (j + 3) ks) + (get (j + 3) blocks)

        t6 =
            s4 + maj3

        e2 =
            a + t5 ~<< 0

        a2 =
            t5 + t6 ~<< 0
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
                            (get 0 blocks) - 1413257819
                    in
                        ( 300032
                          --ab
                        , (t1 - 150054599) ~<< 0
                          --h
                        , (t1 + 24177077) ~<< 0
                          --d
                        )
                else
                    let
                        t2 =
                            (get 0 blocks) - 210244248
                    in
                        ( 704751109
                          --ab
                        , (t2 - 1521486534) ~<< 0
                          --h
                        , (t2 + 143694565) ~<< 0
                        )
                --d
            else
                let
                    s0 =
                        ((hs.a ~>>> 2) ~| (hs.a ~<< 30))
                            ~^ ((hs.a ~>>> 13) ~| (hs.a ~<< 19))
                            ~^ ((hs.a ~>>> 22) ~| (hs.a ~<< 10))

                    s1 =
                        ((hs.e ~>>> 6) ~| (hs.e ~<< 26))
                            ~^ ((hs.e ~>>> 11) ~| (hs.e ~<< 21))
                            ~^ ((hs.e ~>>> 25) ~| (hs.e ~<< 7))

                    ab2 =
                        (hs.a ~& hs.b)

                    maj =
                        ab2 ~^ (hs.a ~& hs.c) ~^ (hs.b ~& hs.c)

                    ch =
                        (hs.e ~& hs.f) ~^ ((lognot hs.e) ~& hs.g)

                    t3 =
                        hs.h + s1 + ch + (get j ks) + (get j blocks)

                    t4 =
                        s0 + maj
                in
                    ( ab2
                      --ab
                    , (hs.d + t3) ~<< 0
                      --h
                    , (t3 + t4) ~<< 0
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
    { a = hs1.a + hs2.a ~<< 0
    , b = hs1.b + hs2.b ~<< 0
    , c = hs1.c + hs2.c ~<< 0
    , d = hs1.d + hs2.d ~<< 0
    , e = hs1.e + hs2.e ~<< 0
    , f = hs1.f + hs2.f ~<< 0
    , g = hs1.g + hs2.g ~<< 0
    , h = hs1.h + hs2.h ~<< 0
    }


outerLoop : HS -> Int -> Int -> Int -> Int -> Bool -> Message -> Int -> HS
outerLoop hs block start bytes index is224 message length =
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
            if (index2 == length) then
                ( orIntoBlocks (i ~>> 2) (getAt (i ~& 3) extra) blocks2
                , index2 + 1
                )
            else
                ( blocks2, index2 )

        block2 =
            get 16 blocks3

        ( end, blocks4 ) =
            if index3 > length && i < 56 then
                ( True, Array.set 15 (bytes2 ~<< 3) blocks3 )
            else
                ( False, blocks3 )

        blocks5 =
            jLoop1 16 blocks4

        first =
            True

        hs2 =
            jLoop2 0 first is224 hs blocks5

        hs3 =
            sumHS hs hs2
    in
        if not end then
            outerLoop hs3 block2 start2 bytes2 index3 is224 message length
        else
            hs3



-- Convert the low 4 bits of a number to a hex character.


toHex1 : Int -> Char
toHex1 x =
    let
        x2 =
            (x ~& 0x0F)
    in
        Char.fromCode
            (x2
                + (if x2 < 10 then
                    Char.toCode ('0')
                   else
                    (-10 + Char.toCode ('a'))
                  )
            )



-- Convert the low 32 bits of an integer to a 4-character hex string.


toHex8 : Int -> String
toHex8 x =
    String.fromList
        (List.map (\shift -> toHex1 (x ~>> shift))
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
    (toHex56 hs) ++ (toHex8 hs.h)


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
            outerLoop hs block start bytes index is224 message length
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
