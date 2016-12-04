module RepeatTypes
    exposing
        ( Section
        , Repeats
        , RepeatState
        , GeneralisedBar
        )

{-| Data types that handle repeated melody sections.  Thesse use a structural data type representing a bar of music
     where the type of notes in the bar can vary with use

# Definition

# Data Types
@docs Section
    , Repeats
    , RepeatState
    , GeneralisedBar

-}

import Music.Accidentals exposing (Accidentals)
import Abc.ParseTree exposing (Repeat(..))


{-| a repeated section
-}
type alias Section =
    { start : Maybe Int
    , firstEnding : Maybe Int
    , secondEnding : Maybe Int
    , end : Maybe Int
    , isRepeated : Bool
    }



{- ! a set of repeats -}


type alias Repeats =
    List Section


{-| the current repeat state
-}
type alias RepeatState =
    { current : Section
    , repeats : Repeats
    }


{-| a parameterised type representing a bar of music where the type of note in the bar varies
-}
type alias GeneralisedBar n =
    { number :
        Int
        -- sequential from zero
    , repeat :
        Maybe Repeat
        -- the bar owns a repeat of some kind
    , iteration :
        Maybe Int
        -- the bar has an iteration marker  (|1  or |2 etc)
    , accidentals :
        Accidentals
        -- any notes marked explicitly as accidentals in the bar (updated in sequence)
    , notes :
        List n
        -- the notes in the bar
    }
