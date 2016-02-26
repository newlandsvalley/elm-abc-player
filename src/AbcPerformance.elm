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

{- note on implementation 

   I originally folded from the right, which is conceptually much simpler and more efficient.
   However, I found that, because of the fact that an explicitly marked accidental influences
   notes of the same pitch class later on in the same bar, I was forced to fold from the left.
   This is more inefficient, as I have to reverse everything at the end.
-}

import Abc.ParseTree exposing (..)
import Abc exposing (ParseError)
import Music.Notation exposing (..)
import String exposing (fromChar, toUpper)
import Ratio exposing (Rational, over, fromInt, toFloat, add)
import Debug exposing (..)

{-| a Note Event (no pitch class implies a rest) -}    
type alias SingleNote = 
  { time : NoteTime
  , pitch : MidiPitch
  , pc : Maybe PitchClass
  , accidental : Maybe Accidental
  }

type NoteEvent =
     ANote SingleNote Bool    -- Bool indicates whether note is tied
   | AChord (List SingleNote)
   
type alias ABar =
  {  number : Int             -- sequential from zero
  ,  repeat : Maybe Repeat    -- the bar owns a repeat of some kind
  ,  iteration : Maybe Int    -- the bar has an iteration marker  (|1  or |2 etc)
  ,  accidentals : KeySet     -- any such notes marked explicitly as accidentals (updated in sequence)
  ,  notes : List NoteEvent   -- the notes in the bar
  }

type alias MelodyLine = List ABar

type alias TranslationState = 
   { keySignature : KeySignature
   , tempo : AbcTempo
   , tempoModifier : Float
   , nextBarNumber : Int
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
  
defaultBar : Int -> ABar
defaultBar i = 
  {  number = i
  ,  repeat = Nothing
  ,  iteration = Nothing
  ,  accidentals = []   
  ,  notes = []
  }

{-
isDefaultBar : ABar -> Bool
isDefaultBar b =
  b == defaultBar
-}

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

{- we need to take note of any accidentals so far in the bar because these may influence
   later notes in that bar.  Build the KeyClass for the accidental of the pitch class in question
   and add it to the list
-}
addNoteToBarAccidentals : SingleNote -> KeySet -> KeySet
addNoteToBarAccidentals n ks =
  case (n.pc, n.accidental) of
    (Just pitchClass, Just acc) ->
      let
        keyClass = (pitchClass, n.accidental)
      in
        if not (List.member keyClass ks) then
          keyClass :: ks
        else
          ks
    _ -> ks

{- ditto for note events (single notes or chords) -}
addNoteEventToBarAccidentals :  NoteEvent -> KeySet -> KeySet
addNoteEventToBarAccidentals ne ks =
  case ne of
   ANote note _ -> addNoteToBarAccidentals note ks 
   AChord ns -> List.foldl (addNoteToBarAccidentals) ks ns

{- ditto for lists of note events -}
addNoteEventsToBarAccidentals :  List NoteEvent -> KeySet -> KeySet
addNoteEventsToBarAccidentals nes ks =
  List.foldl (addNoteEventToBarAccidentals) ks nes
    
{- add a note event to the state - add the note to the growing list of notes in the current bar
   and if the note has an explicit accidental marker, add it to the list of accidentals
-}
addNoteToState : NoteEvent -> TranslationState -> TranslationState 
addNoteToState n state =
  let 
    line = state.thisBar.notes
    thisBar = state.thisBar
    accidentals = addNoteEventToBarAccidentals n thisBar.accidentals
  in
    { state | thisBar = { thisBar | notes = n :: line, accidentals = accidentals } }
  
{- ditto for a list of notes -}
addNotesToState : List NoteEvent -> TranslationState -> TranslationState 
addNotesToState ns state =
  let 
    line = state.thisBar.notes
    thisBar = state.thisBar
    accidentals = addNoteEventsToBarAccidentals ns thisBar.accidentals
  in
    { state | thisBar = { thisBar | notes = List.append ns line, accidentals = accidentals }}

{- translate a sequence of notes as found in chords (parallel) or tuplets (sequential) -}
translateNoteSequence : Bool -> TranslationState -> List AbcNote -> List NoteEvent
translateNoteSequence isSeq state notes =
  let
    f abc = 
      let 
        duration = (noteDuration state.tempo abc.duration) * state.tempoModifier
        barAccidentals = state.thisBar.accidentals
      in
        { time = duration, pitch = toMidiPitch abc state.keySignature barAccidentals, pc = Just abc.pitchClass, accidental = abc.accidental}
  in
    if isSeq then 
       List.map f notes
         |> List.map (\a -> ANote a False)
         |> List.reverse                     -- we need to reverse now we do a left fold
    else 
       [AChord (List.map f notes)]

{- translate a pair of notes, each working under a separate state -}
translateNotePair : AbcNote -> TranslationState -> AbcNote -> TranslationState -> List NoteEvent
translateNotePair n1 s1 n2 s2  =
  let      
    duration1 = (noteDuration s1.tempo n1.duration) * s1.tempoModifier
    duration2 = (noteDuration s2.tempo n2.duration) * s2.tempoModifier
    -- but we'll just accumulate use accidentals in the first state (which will be identical to the second)
    barAccidentals = s1.thisBar.accidentals
    note1 = ANote { time = duration1, pitch = toMidiPitch n1 s1.keySignature barAccidentals, pc = Just n1.pitchClass, accidental = n1.accidental} False
    note2 = ANote { time = duration2, pitch = toMidiPitch n2 s2.keySignature barAccidentals, pc = Just n2.pitchClass, accidental = n2.accidental} False
  in
    -- we add them backwards now because we are doing a left fold
    [note2, note1]

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
          barAccidentals = state.thisBar.accidentals
          note = ANote { time = duration, pitch = toMidiPitch abc state.keySignature barAccidentals, pc = Just abc.pitchClass, accidental = abc.accidental} abc.tied
          newState = addNoteToState note state
        in
          (melodyLine, newState)
      Rest r -> 
        let 
          duration = (noteDuration state.tempo r) * state.tempoModifier
          note = ANote { time = duration, pitch = 0, pc = Nothing, accidental = Nothing } False
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
            if (isEmptyBar state.thisBar) then
              melodyLine
            else
              let
                rb = log "the next bar" state.thisBar
              in
                state.thisBar :: melodyLine
          -- don't increment the bar number if it's an empty bar
          nextBarNumber =   
            if (isEmptyBar state.thisBar) then
               state.nextBarNumber
            else
               state.nextBarNumber + 1

          nextBar = defaultBar nextBarNumber
          newBar = { nextBar | repeat = b.repeat, iteration = b.iteration }
          newState =  { state | thisBar = newBar, nextBarNumber = nextBarNumber }
        in
          (newMelody, newState)
      _ -> acc

-- translate an entire melody line from the tune body (up to an end of line)
toMelodyLine : MusicLine -> (MelodyLine, TranslationState) -> (MelodyLine, TranslationState)
toMelodyLine ml state =
  let 
    (melody, s) = List.foldl translateMusic state ml
  in
    (melody, s)
  
reverseMelody : MelodyLine -> MelodyLine
reverseMelody =
  let 
    reverseBar b = { b | notes = List.reverse b.notes }
  in
    List.map reverseBar
     >> List.reverse
 
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
                        , nextBarNumber = 0
                        , thisBar = defaultBar 0
                        })
    -- update this from the header state if we have any headers
    headerState = List.foldl updateState defaultState (fst tune)
    f bp acc = case bp of
      -- process a line from the melody using the current state
      Score musicLine -> 
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
        (music, state) =  List.foldl f headerState (snd tune)
     -- ensure we don't forget the residual opening bar (still kept in the state) which may yet contain music
     in 
       if (isEmptyBar state.thisBar) then
         reverseMelody music
       else
         reverseMelody (state.thisBar :: music)


fromAbcResult : Result ParseError AbcTune -> Result ParseError MelodyLine
fromAbcResult r =
  Result.map fromAbc r




