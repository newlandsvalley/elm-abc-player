module Lessons
  ( Lesson
  , lessons
  ) where

{-|  Lessons in learning ABC

# Definition

# Data Types
@docs Lesson


# Functions
@docs lessons

-}

import Array exposing (Array, fromList)

type alias Lesson = 
  { title : String
  , instruction : String
  , example : String
  }

instNotes = 
   "Use the characters A-G for the notes of the octave starting from middle C and a-g for the octave above." ++
   " You can place notes next to each other or separate them with spaces - it won't make much difference to " ++
   " the sound but neighbouring notes will be 'beamed' together in a score"

xmplNotes = 
  "A B c def"

instBarsAndRests = 
  "Use the characters Z or z to represent rests and a vertical bar to introduce a bar line." ++
  " Spread out into multiple lines if you like."

xmplBarsAndRests =
  "| ABc z z def |\r\n" ++
  "| g z z z a |"

instOctaves =
  "You can reach octaves below middle C by adding one (or more) commas immediately after the note." ++
  " Similarly higher octaves can be reaced using apostrophes."

xmplOctaves = 
  "| C, G, C G c g c' |"

instLongNotes =
  "So far, all our notes have had a duration of 1 unit.  You can extend this by placing a whole number" ++
  " after the note (and after the octave marker if you have one)."

xmplLongNotes =
  "| F G A B c4 g2 b2 c'4 |"

instShortNotes =
  "You can shorten a note by placing a fraction after the note.  This could be, for example," ++
  "1/2 or 1/3. A shorthand for 1/2 is simply / and a shorthand for 1/3 is simply /3."

xmplShortNotes =
  "| C2 G3/2 C3/2 c/B/A/G/ c4 |"

lessons : Array Lesson 
lessons =
  [
    { title = "the notes", instruction = instNotes, example = xmplNotes }
  , { title = "bars and rests", instruction = instBarsAndRests, example = xmplBarsAndRests }
  , { title = "octaves", instruction = instOctaves, example = xmplOctaves }
  , { title = "long notes", instruction = instLongNotes, example = xmplLongNotes }
  , { title = "short notes", instruction = instShortNotes, example = xmplShortNotes }
  ] |> Array.fromList



