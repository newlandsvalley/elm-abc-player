module Repeats ( Section
               , Repeats
               , RepeatState
               , indexBar 
               , defaultRepeatState
               , finalise
               , buildRepeatedMelody
               ) where

{-|  functions to index repeated RepeatState in an ABC performance and to build a full melody with repeats expanded

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

import Melody exposing (ABar, MelodyLine)
import Abc.ParseTree exposing (Repeat (..))
import Maybe exposing (withDefault) 
import Maybe.Extra exposing (isJust)
import List.Extra exposing (takeWhile, dropWhile)


type alias Section =
  { start : Maybe Int
  , firstEnding : Maybe Int
  , secondEnding : Maybe Int
  , end : Maybe Int
  , isRepeated: Bool
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
        endSection b.number True r
     -- :|:  or ::
     (_, Just BeginAndEnd) ->
        endAndStartSection b.number True True r
     _ ->
       r

{-| accumulate any residual current state from the final bar in the tune -}
finalise : ABar -> RepeatState -> RepeatState
finalise lastBar r =
  let
    -- _ = log "last bar" lastBar 
    -- end the current section with the last bar number
    current = endCurrentSection lastBar.number r.current
    -- fix a degenerate case where we have a repeat indicated by end markers and no begin markers
    -- i.e. this must be the first and also the last (repeated) phrase in the tune
    current1 =     
      if (isEmptyRepeatEndBar lastBar) then
        setRepeated current
      else
        current
    newr = 
      { r | current = current1 }
  in
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
 { start = Just 0, firstEnding  = Nothing, secondEnding  = Nothing, end = Just 0, isRepeated = False }

{- accumulate the last section and start a new section  -}
startSection : Int -> RepeatState -> RepeatState
startSection pos r =
  -- a start implies an end of the last section
  endAndStartSection pos False True r

{- end the section.  If there is a first repeat, keep it open, else accumulate it 
    pos : the bar number marking the end of section
    isRepeatEnd : True if invoked with a known Repeat End marker in the bar line
-}
endSection : Int -> Bool -> RepeatState -> RepeatState
endSection pos isRepeatEnd r =
    if (hasFirstEnding r.current) then
      let
        current = endCurrentSection pos r.current
      in
        { r | current = current }
    else
       endAndStartSection pos isRepeatEnd False r

{- end the current section, accumulate it and start a new section  -}
endAndStartSection : Int -> Bool -> Bool -> RepeatState -> RepeatState
endAndStartSection pos isRepeatEnd isRepeatStart r =
  let 
    -- cater for the situation where the ABC marks the first section of the tune as repeated solely by use
    -- of the End Repeat marker with no such explicit marker at the start of the section - it is implied as the tune start 
    current = 
      if (isRepeatEnd && r.current.start == Just 0) then
        setRepeated r.current
      else
        r.current
    -- now set the end position from the bar number position
    endCurrent = { current | end = Just pos }      
    -- set the entire state and accumulate
    endState = { r | current = endCurrent }
    newState =  accumulateSection endState
    newCurrent = { nullSection | start = Just pos, isRepeated = isRepeatStart }
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

{- set the isRepeated status of a section -}
setRepeated : Section -> Section
setRepeated s =
  { s | isRepeated = True }

{- return True if the section is devoid of any useful content -}
isNullSection : Section -> Bool
isNullSection s = 
  s == nullSection

{- return True if the first (variant) ending is set -}
hasFirstEnding : Section -> Bool
hasFirstEnding s =
  isJust s.firstEnding

{- recognise a solitary bar indicating an end repeat and nothing more - used in finalise -}
isEmptyRepeatEndBar : ABar -> Bool
isEmptyRepeatEndBar b =
  List.length b.notes == 0 && b.repeat == Just End

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

{- build the complete melody with repeated sections in place -}
repeatedSection : MelodyLine -> Section ->  MelodyLine -> MelodyLine
repeatedSection ml s acc  =
  let 
    section { start, firstEnding, secondEnding, end, isRepeated } = (start, firstEnding, secondEnding, end, isRepeated)
  in 
    case (section s) of
      -- variant ending repeat
      (Just a, Just b, Just c, Just d, _ ) ->
         (variantSlice a b c d ml) ++ acc
      -- simple phrase - no repeats
      ( Just a,  _,  _, Just d, False ) ->
         slice a d ml ++ acc
      -- standard repeat
      ( Just a,  _,  _, Just d, True ) ->
         (slice a d ml ++ slice a d ml) ++ acc
      _ -> []


   
    
