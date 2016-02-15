module AbcPerformance (  NoteEvent (..)
                       , MelodyLine
                       , SingleNote
                       , ABar
                       , fromAbc
                       , fromAbcResult
                       ) where

{-|  conversion of a ABC Tune parse tree to a performance

# Definition

# Data Types
@docs NoteEvent, MelodyLine

# Functions
@docs fromAbc, fromAbcResult

-}

import Abc.ParseTree exposing (..)
import Abc exposing (ParseError)
import Music.Notation exposing (..)
import String exposing (fromChar, toUpper)
import Ratio exposing (Rational, over, fromInt, toFloat, add)

{-| a Note Event (no pitch class implies a rest) -}    
type alias SingleNote = 
  { time : NoteTime
  , pitch : MidiPitch
  , pc : Maybe PitchClass
  }

type NoteEvent =
     ANote SingleNote
   | AChord (List SingleNote)
   
type alias ABar =
  {  repeat : Maybe Repeat
  ,  iteration : Maybe Int
  ,  notes : List NoteEvent
  }

type alias MelodyLine = List ABar

type alias TranslationState = 
   { keySignature : KeySignature
   , tempo : AbcTempo
   , tempoModifier : Float
   , thisBar : ABar
   }

-- default to 1/4=120
defaultTempo : AbcTempo
defaultTempo = 
  {  tempoNoteLength = over 1 4
  ,  bpm = 120
  ,  unitNoteLength = over 1 8
  }

-- default to C Major
defaultKey : KeySignature
defaultKey = 
  { pitchClass = C
  , accidental = Nothing
  , mode = Major
  } 
  
defaultBar : ABar
defaultBar = 
  {  repeat = Nothing
  ,  iteration = Nothing
  ,  notes = []
  }

isDefaultBar : ABar -> Bool
isDefaultBar b =
  b == defaultBar

isEmptyBar : ABar -> Bool
isEmptyBar b =
  List.length b.notes == 0

-- get the tempo from the tune header
{-
getHeaderTempo : AbcTempo -> TuneHeaders -> AbcTempo
getHeaderTempo a =
  let
    f h acc = 
      case h of
        UnitNoteLength d ->
           { acc | unitNoteLength = d }
        Tempo t ->
          let 
            tnl = List.foldl Ratio.add (fromInt 0) t.noteLengths
          in
           { acc | tempoNoteLength = tnl, bpm = t.bpm }
        _ -> acc       
  in
    List.foldr f a
-}

{- update the state of the player when we come across a header (either at the start or inline)
   which affects the tune tempo or the pitch of a note (i.e. they key)
-}
updateState : Header -> (MelodyLine, TranslationState) -> (MelodyLine, TranslationState)
updateState h acc =
  let 
    (melody, state) = acc
    tempo = state.tempo
  in case h of
    UnitNoteLength d ->
      (melody, { state | tempo = { tempo | unitNoteLength = d }} )
    Tempo t ->
      let 
        tnl = List.foldl Ratio.add (fromInt 0) t.noteLengths
      in
       (melody, { state | tempo = { tempo | tempoNoteLength = tnl, bpm = t.bpm }} )
    -- ignore accidental note modifiers in key signatures for the moment - they're little used
    Key k accs ->
       (melody, { state | keySignature = k} )
    _ -> acc       
    
addNoteToState : NoteEvent -> TranslationState -> TranslationState 
addNoteToState n state =
  let 
    line = state.thisBar.notes
    thisBar = state.thisBar
  in
    { state | thisBar = { thisBar | notes = n :: line }}
  
addNotesToState : List NoteEvent -> TranslationState -> TranslationState 
addNotesToState ns state =
  let 
    line = state.thisBar.notes
    thisBar = state.thisBar
  in
    { state | thisBar = { thisBar | notes = List.append ns line }}

{- translate a sequence of notes as found in chords (parallel) or tuplets (sequential) -}
translateNoteSequence : Bool -> TranslationState -> List AbcNote -> List NoteEvent
translateNoteSequence isSeq state notes =
  let
    f abc = 
      let 
        duration = (noteDuration state.tempo abc.duration) * state.tempoModifier
      in
        { time = duration, pitch = toMidiPitch abc state.keySignature, pc = Just abc.pitchClass}
  in
    if isSeq then 
       List.map f notes
         |> List.map (\a -> ANote a)
    else 
       [AChord (List.map f notes)]

{- translate a pair of notes, each working under a separate state -}
translateNotePair : AbcNote -> TranslationState -> AbcNote -> TranslationState -> List NoteEvent
translateNotePair n1 s1 n2 s2  =
  let      
    duration1 = (noteDuration s1.tempo n1.duration) * s1.tempoModifier
    duration2 = (noteDuration s2.tempo n2.duration) * s2.tempoModifier
    note1 = ANote { time = duration1, pitch = toMidiPitch n1 s1.keySignature, pc = Just n1.pitchClass} 
    note2 = ANote { time = duration2, pitch = toMidiPitch n2 s2.keySignature, pc = Just n2.pitchClass} 
  in
    [note1, note2]

{- not at all complete - translate Music items from the parse tree to a melody line - a sequence
   of bars containing notes, rests and chords where notes are in a MIDI-friendly format
-}
translateMusic : Music -> (MelodyLine, TranslationState) -> (MelodyLine, TranslationState)
translateMusic m acc =
  let 
    (melodyLine, state) = acc
  in
    case m of
      Note abc -> 
        let 
          duration = (noteDuration state.tempo abc.duration) * state.tempoModifier
          note = ANote { time = duration, pitch = toMidiPitch abc state.keySignature, pc = Just abc.pitchClass} 
          newState = addNoteToState note state
        in
          (melodyLine, newState)
      Rest r -> 
        let 
          duration = (noteDuration state.tempo r) * state.tempoModifier
          note = ANote { time = duration, pitch = 0, pc = Nothing }
          newState = addNoteToState note state
        in 
          (melodyLine, newState)
      Tuplet signature tnotes ->
        let 
          (p,q,r) = signature
          tupletState = { state | tempoModifier = ( Basics.toFloat q / Basics.toFloat p) }
          tupletNotes = translateNoteSequence True tupletState tnotes
          newState = addNotesToState tupletNotes state
        in 
          (melodyLine, newState)
      BrokenRhythmPair n1 b n2 ->     
        case b of 
          LeftArrow i ->
            let
              leftState =  { state | tempoModifier = ( 1 - dotFactor i) }
              rightState =  { state | tempoModifier = ( 1 + dotFactor i) }
              notePair = translateNotePair n1 leftState n2 rightState
              newState = addNotesToState notePair state
            in              
              (melodyLine, newState)
          RightArrow i ->
            let
              leftState =  { state | tempoModifier = ( 1 + dotFactor i) }
              rightState =  { state | tempoModifier = ( 1 - dotFactor i) }
              notePair = translateNotePair n1 leftState n2 rightState
              newState = addNotesToState notePair state
            in              
              (melodyLine, newState)
      Chord abcChord ->
        let 
          chord = translateNoteSequence False state abcChord.notes
          newState = addNotesToState chord state
        in             
          (melodyLine, newState)
      Barline b ->
        let 
          -- don't add to the melody the existing bar accumulated by the state if it's empty
          newMelody = 
            if (isDefaultBar state.thisBar) then
              melodyLine
            else
              state.thisBar :: melodyLine
          newBar = { defaultBar | repeat = b.repeat, iteration = b.iteration }
          newState =  { state | thisBar = newBar }
        in
          (newMelody, newState)
      _ -> acc

-- translate an entire melody line from the tune body (up to an end of line)
toMelodyLine : MusicLine -> (MelodyLine, TranslationState) -> (MelodyLine, TranslationState)
toMelodyLine ml state =
  List.foldr translateMusic state ml
  
 
{- translate an AbcTune to a more playable melody line
   which is a list of notes (or rests) and their durations
-}
fromAbc : AbcTune -> MelodyLine
fromAbc tune =   
  let
    -- set a default state for case where there are no tune headers
    defaultState = ([], { keySignature = defaultKey
                        , tempo = defaultTempo
                        , tempoModifier = 1.0
                        , thisBar = defaultBar
                        })
    -- update this from the header state if we have any headers
    headerState = List.foldl updateState defaultState (fst tune)
    f bp acc = case bp of
      -- process a line from the melody using the current state
      Score musicLine continuation -> 
        let 
          (existingLine, state) = acc
          (newLine, newState) = toMelodyLine musicLine acc
        in
          (newLine, newState)
      -- update the state if we have an inline header
      BodyInfo header -> 
        updateState header acc
   in 
     let
        (music, state) =  List.foldr f headerState (snd tune)
     -- ensure we don't forget the residual opening bar (still kept in the state) which may yet contain music
     in 
       if (isEmptyBar state.thisBar) then
         music
       else
         (state.thisBar :: music)


fromAbcResult : Result ParseError AbcTune -> Result ParseError MelodyLine
fromAbcResult r =
  Result.map fromAbc r




