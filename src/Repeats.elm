module Repeats ( Section
               , Repeats
               , RepeatState
               , indexBar 
               , defaultRepeatState
               , finalise
               ) where

{-|  functions to index repeated RepeatState in an ABC performance 

     for example:

       |: .... :|    
         -> Section ( start .. end),  section ( start .. end)

     or
       |: .... |1 ... :|2 .... ||     
          -> Section ( start 1 2 end)

# Definition

# Data Types
@docs Section, RepeatState

# Functions
@docs indexBar

-}

import Performance exposing (ABar)
import Abc.ParseTree exposing (Repeat (..))
import Maybe.Extra exposing (isJust)

type alias Section =
  { start : Maybe Int
  , firstEnding : Maybe Int
  , secondEnding : Maybe Int
  , end : Maybe Int
  }

type alias Repeats = List Section

type alias RepeatState =
  {  current : Section
  ,  repeats : Repeats
  }

{- a 'null' section -}
nullSection : Section
nullSection =
 { start = Just 0, firstEnding  = Nothing, secondEnding  = Nothing, end = Just 0 }

{- accumulate the last section and start a new section  -}
startSection : Int -> RepeatState -> RepeatState
startSection pos r =
  -- a start implies an end of the last section
  endAndStartSection pos pos r

{-
startSection pos r =
  let 
    newState =  accumulateSection r
    newCurrent = { nullSection | start = Just pos }
  in
    { newState | current = newCurrent }
-}

{- end the section.  If there is a first repeat, keep it open, else accumulate it -}
endSection : Int -> RepeatState -> RepeatState
endSection pos r =
    if (hasFirstEnding r.current) then
      let
        current = endCurrentSection pos r.current
      in
        { r | current = current }
    else
       endAndStartSection pos 0 r

{- end the current section, accumulate it and start a new section  -}
endAndStartSection : Int -> Int -> RepeatState -> RepeatState
endAndStartSection endpos startpos r =
  let 
    current = r.current
    endCurrent = { current | end = Just endpos }
    endState = { r | current = endCurrent }
    newState =  accumulateSection endState
    newCurrent = { nullSection | start = Just startpos }
  in
    { newState | current = newCurrent }
 
{- set the end of the current section -}
endCurrentSection : Int -> Section -> Section
endCurrentSection pos s =
  { s | end = Just pos } 

{- set the first repeat of a section -}
firstRepeat : Int -> Section -> Section
firstRepeat pos s =
  { s | firstEnding = Just pos } 

{- set the second repeat of a section -}
secondRepeat : Int -> Section -> Section
secondRepeat pos s =
  { s | secondEnding = Just pos }

isNullSection : Section -> Bool
isNullSection s = 
  s == nullSection

hasFirstEnding : Section -> Bool
hasFirstEnding s =
  isJust s.firstEnding


{- accumulate the current section into the full score and re-initialise it -}
accumulateSection : RepeatState -> RepeatState
accumulateSection r =
    if not (isNullSection r.current) then
         {r | repeats = r.current :: r.repeats, current = nullSection }
    else
      r

{-| repeats default to no repeats (yet) -}
defaultRepeatState : RepeatState
defaultRepeatState =
  { current = nullSection, repeats = [] }


{-| index a bar by identifying any repeat markings and saving the marking against the bar number -}
indexBar : ABar -> RepeatState -> RepeatState
indexBar b r = 
  case (b.iteration, b.repeat) of
     -- |1  
     (Just 1, _) ->
        {r | current = firstRepeat b.number r.current}
     -- |2  or :|2
     (Just 2, _) ->
        {r | current = secondRepeat b.number r.current}
     -- |:
     (_, Just Begin) ->    
        startSection b.number r
     -- :|
     (_, Just End) ->       
        endSection b.number r
     -- :|:  or ::
     (_, Just BeginAndEnd) ->
        endAndStartSection b.number b.number r
     _ ->
       r

{-| accumulate any residual current state -}
finalise : ABar -> RepeatState -> RepeatState
finalise lastBar r =
  let
     newr = indexBar lastBar r
  in
    if (isNullSection newr.current) then
      newr
    else
      accumulateSection newr
    
