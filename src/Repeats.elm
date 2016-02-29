module Repeats ( Section
               , Repeats
               , RepeatState
               , indexBar 
               , defaultRepeatState
               , finalise
               , buildRepeatedMelody
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
@docs Section, Repeats, RepeatState

# Functions
@docs indexBar 
    , defaultRepeatState
    , finalise
    , buildRepeatedMelody

-}

import Performance exposing (ABar, MelodyLine)
import Abc.ParseTree exposing (Repeat (..))
import Maybe.Extra exposing (isJust)
import List.Extra exposing (takeWhile, dropWhile)

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


{-| default repeats i.e. no repeats yet -}
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
     -- this is the last bar - if we're ending on a repeat variant, then we need to terminate it properly
     current = r.current
     -- mark the last bar with the end repeat if we're in a variant ending
     newLastBar =
       if (isJust current.firstEnding) then
          { lastBar | repeat = Just End }
       else
         lastBar
     -- and then index the last bar
     newr = indexBar newLastBar r    
  in
    -- nothing of interest in current so just return it
    if (isNullSection newr.current) then
      newr
    else 
    -- something of interest in current so accummulate it and return it
      accumulateSection newr

{-| build any repeated section into an extended melody with all repeats realised -}
buildRepeatedMelody : (MelodyLine, Repeats) -> MelodyLine
buildRepeatedMelody (ml, repeats) =
  if (List.isEmpty repeats) then
    ml
  else
    List.foldr (repeatedSection ml) [] repeats

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

{-| take a slice of a melody line between start and finish -}
slice : Int -> Int -> MelodyLine -> MelodyLine
slice start end =
  dropWhile (\bar -> bar.number < start)
    >> takeWhile (\bar -> bar.number < end)

{-| take two variant slices of a melody line between start and finish 
    taking account of first repeat and second repeat sections
-}
variantSlice : Int -> Int -> Int -> Int -> MelodyLine -> MelodyLine
variantSlice start firstRepeat secondRepeat end ml =
  let 
    section = slice start end ml
    firstSection = slice start secondRepeat section
    secondSection = slice start firstRepeat section ++ slice secondRepeat end section
  in
    firstSection ++ secondSection

repeatedSection : MelodyLine -> Section ->  MelodyLine -> MelodyLine
repeatedSection ml s acc  =
  let 
    section { start, firstEnding, secondEnding, end } = (start, firstEnding, secondEnding, end)
  in 
    case (section s) of
      (Just a, Just b, Just c, Just d ) ->
         (variantSlice a b c d ml) ++ acc
      ( Just a,  _,  _, Just d ) ->
         (slice a d ml ++ slice a d ml) ++ acc
      _ -> []


   
    
