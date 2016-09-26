module Combine.Extra
    exposing
        ( manyTill'
        , leftBiasedOr
        )

{-| Extension functions to elm-combine to allow custom error-reporting

# Definition

# Functions
@docs manyTill'
    , leftBiasedOr

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)


{- Provide a version of manyTill that preserves the error position of the 'many' rather than of the 'end'
   Many thanks to Bogdan Papa for helping me with this

   manyTill' Parser res -> Parser end -> Parser (List res)
-}


manyTill' p end =
    let
        accumulate acc cx =
            case app end cx of
                -- We've reached the end so we return the accumulated results
                -- and the context _after_ running the `end` parser.
                ( Ok _, rcx ) ->
                    ( Ok (List.reverse acc), rcx )

                ( Err _, origcx ) ->
                    case app p cx of
                        -- Our parser succeeded so we loop
                        ( Ok res, rcx ) ->
                            accumulate (res :: acc) rcx

                        -- Our parser failed so we fail the whole thing. This is
                        -- where this implementation differs from the standard
                        -- `manyTill`, we return `p`'s error rather than `end`'s.
                        ( Err ms, ecx ) ->
                            ( Err ms, ecx )

        {-
           let
             mcx = log "original ctx" origcx
           in
             (Err (log "mt' msg" ms), (log "mt' ctx" ecx))
        -}
    in
        primitive <| accumulate []



{- Provide a version of or that preserves the error position of the left hand branch rather than
   the start of the 'orred' construction.  Used to ensure manyTill' error positions filter up through 'or'

   leftBiasedOr : Parser res -> Parser res -> Parser res
-}


leftBiasedOr lp rp =
    primitive <|
        \cx ->
            let
                res =
                    app lp cx
            in
                case res of
                    ( Ok _, _ ) ->
                        res

                    ( Err lm, lcx ) ->
                        let
                            res' =
                                app rp cx
                        in
                            case res' of
                                ( Ok _, _ ) ->
                                    res'

                                ( Err rm, rcx ) ->
                                    ( Err (lm ++ rm), lcx )



{-
   let
     mcx = log "original or ctx" cx
   in
     -- preserve lcx rather than cx
     (Err (lm ++ rm), log "left" lcx)
-}
