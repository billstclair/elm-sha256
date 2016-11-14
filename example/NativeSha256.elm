----------------------------------------------------------------------
--
-- NativeSha256.elm
-- sha256 & sha224 for Elm using Native JavaScript
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module NativeSha256 exposing (sha256, sha224)

{-| This module makes  available a JavaScript implementation of the
sha256 and sha224 crytographic hash functions.

Thank you to Yi-Cyuan Chen for writing the JavaScript.

# Functions
@docs sha256, sha224
-}

import Native.Sha256


{-| Returns the sha256 hash of its argument.
-}
sha256 : String -> String
sha256 =
    Native.Sha256.sha256


{-| Returns the sha224 hash of its argument.
-}
sha224 : String -> String
sha224 =
    Native.Sha256.sha224
