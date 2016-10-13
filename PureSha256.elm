----------------------------------------------------------------------
--
-- sha256.elm
-- sha256 & sha224 for Elm
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module PureSha256 exposing ( sha256
                           , sha224
                            -- for debugging
                           , toHex1, toHex8, toHex56, toHex64  
                           , hash, outerLoop, indexLoop, jLoop1, jLoop2
                           , makeBlocks, stringToCodes, initialHs
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
import Bitwise exposing (and, or, shiftLeft, shiftRight, shiftRightLogical)
import Debug exposing (log)

extra : List Int
extra = [-2147483648, 8388608, 32768, 128]

k : Array Int
k =
  Array.fromList
    [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
    , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
    , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
    , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
    , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
    , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
    , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
    , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    ]

get : Int -> Array Int -> Int
get index array =
  -- The default is actually an error, but I don't expect it to happen.
  Maybe.withDefault 0 (Array.get index array)

orIntoArray : Int -> Int -> Array Int -> Array Int
orIntoArray idx val array =
  Array.set idx (or val (get idx array)) array

getAt : Int -> List Int -> Int
getAt index list =
  Maybe.withDefault 0 (LE.getAt index list)

logxor : Int -> Int -> Int
logxor x y =
  Bitwise.xor x y

-- Done with SHIFT array in JavaScript code
-- SHIFT = [24, 16, 8, 0]
getShift : Int -> (Int -> Int)
getShift i =
  (\n -> 8 * (3 - (and (i + n) 3)))

indexLoop : Int -> Int -> Array Int -> Int -> Array Int -> (Int, Int, Array Int)
indexLoop i index message length blocks =
  if i > 64 || index >= length then
    (i, index, blocks)
  else
    let code = (get index message)
        shift = getShift i
    in
        let (iInc, idxInc, val) =
              if code < 0x80 then
                ( 1
                , 0
                , shiftLeft code (shift 0)
                )
              else if code < 0x800 then
                ( 2
                , 0
                , or (shiftLeft (or 0xc0 (shiftRight code 6)) (shift 0))
                     (shiftLeft (or 0x80 (and code 0x3f)) (shift 1))
                )
              else if code < 0xd800 || code >= 0xe000 then
                ( 3
                , 0
                , or (shiftLeft (or 0xe0 (shiftRight code 12)) (shift 0))
                     (shiftLeft
                        (or 0x80 (and (shiftRight code 6) 0x3f))
                        (shift 1))
                    |> or (shiftLeft (or 0x80 (and code 0x3f)) (shift 2))
                )
              else
                ( 4
                , 1
                , let code2 = (+) 0x10000
                                  (or (shiftLeft (and code 0x3ff) 10)
                                      (and (get (index+1) message) 0x3ff))
                  in
                      or (shiftLeft (or 0xf0 (shiftRight code2 18)) (shift 0))
                         (shiftLeft
                            (or 0x80 (and (shiftRight code2 12) 0x3f))
                            (shift 1))
                        |> or (shiftLeft
                                 (or 0x80 (and (shiftRight code2 6) 0x3f))
                                 (shift 2))
                        |> or (shiftLeft (or 0x80 (and code2 0x3f)) (shift 3))
                )
        in
            let 
                blocks2 = orIntoArray (shiftRight i 2) val blocks
            in
                indexLoop (i + iInc) (index + idxInc + 1) message length blocks2

type alias HS =
  { h0 : Int
  , h1 : Int
  , h2 : Int
  , h3 : Int
  , h4 : Int
  , h5 : Int
  , h6 : Int
  , h7 : Int
  }

makeBlocks : Int -> Array Int
makeBlocks block =
  Array.append (Array.fromList [block]) (Array.repeat 16 0)
    
jLoop1 : Int -> (Array Int) -> (Array Int)
jLoop1 j blocks =
  let t1 = get (j-15) blocks
      t2 = get (j-2) blocks
  in
      let s0 = or (shiftRightLogical t1 7) (shiftLeft t1 25)
                 |> logxor (or (shiftRightLogical t1 18)
                               (shiftLeft t1 14))
                 |> logxor (shiftRightLogical t1 3)
          s1 = or (shiftRightLogical t2 17) (shiftLeft t2 25)
                 |> logxor (or (shiftRightLogical t1 18)
                               (shiftLeft t1 14))
                 |> logxor (shiftRightLogical t1 10)
      in
          let blocks2 = Array.set
                        j
                        (shiftLeft ((get (j-16) blocks) +
                                    s0 +
                                    (get (j-7) blocks) +
                                    s1)
                                   0)
                        blocks
          in
              if j >= 63 then
                blocks2
              else
                jLoop1 (j+1) blocks2

jLoop2 : Int -> HS -> (Array Int) -> HS
jLoop2 j hs blocks =
  hs
{-
              a = hs.h0
              b = hs.h1
              c = hs.h2
              d = hs.h3
              e = hs.h4
              f = hs.h5
              g = hs.h6
              h = hs.h7
                  bc = and b c
                               for (j = 0;j < 64;j += 4) {
                                     if (first) {
                                           if (is224) {
                                                 ab = 300032;
                                                   t1 = blocks[0] - 1413257819;
                                                   h = t1 - 150054599 << 0;
                                                   d = t1 + 24177077 << 0;
                                             } else {
                                                   ab = 704751109;
                                                     t1 = blocks[0] - 210244248;
                                                     h = t1 - 1521486534 << 0;
                                                     d = t1 + 143694565 << 0;
                                               }
                                             first = false;
                                       } else {
                                             s0 = ((a >>> 2) | (a << 30)) ^ ((a >>> 13) | (a << 19)) ^ ((a >>> 22) | (a << 10));
                                               s1 = ((e >>> 6) | (e << 26)) ^ ((e >>> 11) | (e << 21)) ^ ((e >>> 25) | (e << 7));
                                               ab = a & b;
                                               maj = ab ^ (a & c) ^ bc;
                                               ch = (e & f) ^ (~e & g);
                                               t1 = h + s1 + ch + K[j] + blocks[j];
                                               t2 = s0 + maj;
                                               h = d + t1 << 0;
                                               d = t1 + t2 << 0;
                                         }
                                       s0 = ((d >>> 2) | (d << 30)) ^ ((d >>> 13) | (d << 19)) ^ ((d >>> 22) | (d << 10));
                                       s1 = ((h >>> 6) | (h << 26)) ^ ((h >>> 11) | (h << 21)) ^ ((h >>> 25) | (h << 7));
                                       da = d & a;
                                       maj = da ^ (d & b) ^ ab;
                                       ch = (h & e) ^ (~h & f);
                                       t1 = g + s1 + ch + K[j + 1] + blocks[j + 1];
                                       t2 = s0 + maj;
                                       g = c + t1 << 0;
                                       c = t1 + t2 << 0;
                                       s0 = ((c >>> 2) | (c << 30)) ^ ((c >>> 13) | (c << 19)) ^ ((c >>> 22) | (c << 10));
        s1 = ((g >>> 6) | (g << 26)) ^ ((g >>> 11) | (g << 21)) ^ ((g >>> 25) | (g << 7));
        cd = c & d;
        maj = cd ^ (c & a) ^ da;
        ch = (g & h) ^ (~g & e);
        t1 = f + s1 + ch + K[j + 2] + blocks[j + 2];
        t2 = s0 + maj;
        f = b + t1 << 0;
        b = t1 + t2 << 0;
        s0 = ((b >>> 2) | (b << 30)) ^ ((b >>> 13) | (b << 19)) ^ ((b >>> 22) | (b << 10));
        s1 = ((f >>> 6) | (f << 26)) ^ ((f >>> 11) | (f << 21)) ^ ((f >>> 25) | (f << 7));
        bc = b & c;
        maj = bc ^ (b & d) ^ cd;
        ch = (f & g) ^ (~f & h);
        t1 = e + s1 + ch + K[j + 3] + blocks[j + 3];
        t2 = s0 + maj;
        e = a + t1 << 0;
        a = t1 + t2 << 0;
      }

      h0 = h0 + a << 0;
      h1 = h1 + b << 0;
      h2 = h2 + c << 0;
      h3 = h3 + d << 0;
      h4 = h4 + e << 0;
      h5 = h5 + f << 0;
      h6 = h6 + g << 0;
      h7 = h7 + h << 0;
-}

outerLoop : HS -> Int -> Int -> Int -> Int -> Array Int -> Int -> HS
outerLoop hs block start bytes index message length =
  let (i, index2, blocks) = indexLoop start index message length (makeBlocks block)
  in
      let bytes2 = bytes + i - start
          start2 = i - 64
          (blocks2, index3) =
            if (index2 == length) then
              ( orIntoArray (shiftRight i 2) (getAt (and i 3) extra) blocks
              , index2 + 1)
            else
              (blocks2, index2)
      in
          let block2 = get 16 blocks2
              (end, blocks3) = if index > length && i < 56 then
                                 (True, Array.set 15 (shiftLeft bytes2 3) blocks2)
                               else
                                 (False, blocks)
          in
              let blocks4 = jLoop1 16 blocks3
              in
                  let hs2 = jLoop2 0 hs blocks4
                  in
                      if end then
                        hs2
                      else
                        outerLoop hs2 block2 start2 bytes2 index3 message length

-- Convert the low 4 bits of a number to a hex character.
toHex1 : Int -> Char
toHex1 x =
  let x2 = (and x 0xf)
  in
      Char.fromCode (x2 + (if x2 < 10
                           then Char.toCode('0')
                           else (-10 + Char.toCode('a'))))

-- Convert the low 32 bits of an integer to a 4-character hex string.
toHex8 : Int -> String
toHex8 x =
  String.fromList (List.map (\shift -> toHex1 (shiftRight x shift))
                            [28, 24, 20, 16, 12, 8, 4, 0])

-- Convert 7 elements of an HS instance to a 56-character hex string.
toHex56 : HS -> String
toHex56 hs =
  List.foldr
    (++)
    ""
    (List.map
       toHex8
       [hs.h0, hs.h1, hs.h2, hs.h3, hs.h4, hs.h5, hs.h6])

-- Convert 8 elements of an HS instance to a 64-character hex string.
toHex64 : HS -> String
toHex64 hs =
  (toHex56 hs) ++ (toHex8 hs.h7)

stringToCodes: String -> Array Int
stringToCodes string =
  Array.fromList (List.map Char.toCode (String.toList string))

initialHs : Bool -> HS
initialHs is224 =
  if is224 then
    -- sha224
    { h0 = 0xc1059ed8
    , h1 = 0x367cd507
    , h2 = 0x3070dd17
    , h3 = 0xf70e5939
    , h4 = 0xffc00b31
    , h5 = 0x68581511
    , h6 = 0x64f98fa7
    , h7 = 0xbefa4fa4
    }
  else
    -- sha256
    { h0 = 0x6a09e667
    , h1 = 0xbb67ae85
    , h2 = 0x3c6ef372
    , h3 = 0xa54ff53a
    , h4 = 0x510e527f
    , h5 = 0x9b05688c
    , h6 = 0x1f83d9ab
    , h7 = 0x5be0cd19
    }
    
hash : String -> Bool -> String
hash string is224 =
  let hs = initialHs is224
      block = 0
      start = 0
      bytes = 0
      index = 0
      message = stringToCodes string
      length = Array.length message
  in
      let hs2 = outerLoop hs block start bytes index message length
      in
          if is224 then
            toHex56 hs2
          else
            toHex64 hs2

{-| Returns the sha256 hash of its argument.
-}
sha256 : String -> String
sha256 message =
  hash (log "message" message) False

{-| Returns the sha224 hash of its argument.
-}
sha224 : String -> String
sha224 message =
  hash message True
