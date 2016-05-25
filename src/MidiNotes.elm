module MidiNotes exposing (..) 

import SoundFont.Types exposing (MidiNote, MidiNotes)
import Notable exposing (..)

{- module that transforms an AbcPerformance into MidiNotes -}

{- make the next MIDI note -}
makeMIDINote : (Float, Notable) -> MidiNote
makeMIDINote ne = 
  let 
    (time, notable) = ne
  in
    case notable of
       -- we've hit a Note
       Note pitch velocity ->
         MidiNote pitch time velocity

{- make the MIDI notes - if we have a performance result from parsing the midi file, convert
   the performance into a list of MidiNote
-}
makeMIDINotes :  Result error Performance -> MidiNotes
makeMIDINotes perfResult = 
  case perfResult of
    Ok perf ->
      List.map makeMIDINote perf
    Err err ->
      []

{- calculate the phrase duration in seconds from the reversed MidiNotes sequence -}
reversedPhraseDuration : MidiNotes -> Float
reversedPhraseDuration notes =
  let
    maybeLastNote = List.head notes
  in 
    case maybeLastNote of
      Nothing -> 0.0
      Just n -> n.timeOffset  -- the accumulated time

