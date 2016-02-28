module Performance ( NoteEvent (..)
                   , MelodyLine
                   , SingleNote
                   , ABar
                   ) where

{-|  Data structures for describing an ABC performance

# Definition

# Data Types
@docs NoteEvent, MelodyLine, SingleNote, ABar


-}

import Abc.ParseTree exposing (PitchClass, Accidental, Repeat)
import Music.Notation exposing (NoteTime, KeySet, MidiPitch)

{-| an individual Note (no pitch class implies a rest) -}    
type alias SingleNote = 
  { time : NoteTime
  , pitch : MidiPitch
  , pc : Maybe PitchClass
  , accidental : Maybe Accidental
  }

{-| a Note Event (note or chord) -}    
type NoteEvent =
     ANote SingleNote Bool    -- Bool indicates whether note is tied
   | AChord (List SingleNote)
   
{- A Bar -}
type alias ABar =
  {  number : Int             -- sequential from zero
  ,  repeat : Maybe Repeat    -- the bar owns a repeat of some kind
  ,  iteration : Maybe Int    -- the bar has an iteration marker  (|1  or |2 etc)
  ,  accidentals : KeySet     -- any such notes marked explicitly as accidentals (updated in sequence)
  ,  notes : List NoteEvent   -- the notes in the bar
  }

{- the overall melody -}
type alias MelodyLine = List ABar


