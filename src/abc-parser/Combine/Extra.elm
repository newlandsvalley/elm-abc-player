module Combine.Extra 
   ( manyTill' )
   where

{-|  Extension functions to elm-combine to allow custom error-reporting

# Definition

# Functions
@docs manyTill'

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Debug exposing (..)

manyTill' p end =
  let
    accumulate acc cx =
      case app end cx of
        -- We've reached the end so we return the accumulated results
        -- and the context _after_ running the `end` parser.
        (Ok _, rcx) ->
          (Ok (List.reverse acc), rcx)

        (Err _, origcx) ->
          case app p cx of
            -- Our parser succeeded so we loop
            (Ok res, rcx) ->
              accumulate (res :: acc) rcx

            -- Our parser failed so we fail the whole thing. This is
            -- where this implementation differs from the standard
            -- `manyTill`, we return `p`'s error rather than `end`'s.
            (Err ms, ecx) ->
              let 
                mcx = log "original ctc" origcx
              in
                (Err (log "mt' msg" ms), (log "mt ctx" ecx))
  in
    primitive <| accumulate []

