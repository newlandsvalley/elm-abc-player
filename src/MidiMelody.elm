module MidiMelody exposing
  ( MidiInstruction (..)
  , MidiMelody
  , MidiNote
  , MidiBar
  ) 

{-|  Data structures for describing an ABC melody  (i.e just a succession of notes and durations)

# Definition

# Data Types
@docs MidiInstruction, MidiMelody, MidiNote, MidiBar

-}

import Abc.ParseTree exposing (PitchClass, Accidental, KeySet, Repeat)
import Music.Notation exposing (NoteTime, MidiPitch, MidiTick)
import Music.Accidentals exposing (Accidentals)
import RepeatTypes exposing (GeneralisedBar)

{-| an individual Note (no pitch class implies a rest) -}    
type alias MidiNote = 
  { ticks : MidiTick
  , pitch : MidiPitch
  , pc : Maybe PitchClass
  , accidental : Maybe Accidental
  }

{-| a Midi Instruction (not, chord or tempo) -}    
type MidiInstruction =
     MNote MidiNote Bool    -- Bool indicates whether note is tied
   | MChord (List MidiNote)
   | MTempo Int             -- Tempo in microseconds per unit beat - we wrap this up in a bar of its own
   
{-| A Bar -}
type alias MidiBar =
  GeneralisedBar MidiInstruction

{- GeneralisedBar is a parameterised type defined in RepeatTypes.elm
   which makes MidiBar (when expanded) look like this:
type alias MidiBar =
  {  number : Int               -- sequential from zero
  ,  repeat : Maybe Repeat      -- the bar owns a repeat of some kind
  ,  iteration : Maybe Int      -- the bar has an iteration marker  (|1  or |2 etc)
  ,  accidentals : Accidentals  -- any notes marked explicitly as accidentals in the bar (updated in sequence)
  ,  notes : List MidiNote      -- the notes in the bar
  }
-}

{- the overall melody -}
type alias MidiMelody = List MidiBar


