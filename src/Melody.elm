module Melody exposing
  ( NoteEvent (..)
  , MelodyLine
  , SingleNote
  , ABar
  ) 

{-|  Data structures for describing an ABC melody  (i.e just a succession of notes and durations)

# Definition

# Data Types
@docs NoteEvent, MelodyLine, SingleNote, ABar

-}

import Abc.ParseTree exposing (PitchClass, Accidental, KeySet, Repeat)
import Music.Notation exposing (NoteTime, MidiPitch)
import Music.Accidentals exposing (Accidentals)
import RepeatTypes exposing (GeneralisedBar)

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
   GeneralisedBar NoteEvent

{-
type alias ABar =
  {  number : Int               -- sequential from zero
  ,  repeat : Maybe Repeat      -- the bar owns a repeat of some kind
  ,  iteration : Maybe Int      -- the bar has an iteration marker  (|1  or |2 etc)
  ,  accidentals : Accidentals  -- any notes marked explicitly as accidentals in the bar (updated in sequence)
  ,  notes : List NoteEvent     -- the notes in the bar
  }
-}

{- the overall melody -}
type alias MelodyLine = List ABar


