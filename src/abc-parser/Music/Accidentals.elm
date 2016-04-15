module Music.Accidentals
  ( Accidentals
  , empty
  , add
  , fromKeySet
  , lookup
  , lookupNote
  ) where
  
  
{-|  module to provide a type-safe interface into handling
    sets of Key Accidentals - i.e. pitch classes associated
    with an explicit sharp, flat or natural modifier.
    
    (as of elm 0.16, type-safety of Dicts is compromised)


# Definition

# Data Types
@docs Accidentals

# Functions
@docs empty
    , add
    , fromKeySet
    , lookup
    , lookupNote

-}

import Abc.ParseTree exposing (AbcNote, PitchClass (..), Accidental (..), KeyAccidental, KeySet)
import Dict exposing (Dict, fromList, get)

{-| A set of accidentals,  String is a proxy for PitchClass -}
type alias Accidentals = Dict String Accidental
 
{-| create an empty set of Key Accidentals -}
empty : Accidentals
empty = Dict.empty

{-| add an accidental to the set -}
add : PitchClass -> Accidental -> Accidentals -> Accidentals
add pc acc accs =
  Dict.insert (toString pc) acc accs
  
{-| build Accidentals from a KeySet -}
fromKeySet : KeySet -> Accidentals
fromKeySet ks =
  let
    f (pc, acc) = (toString pc, acc) 
    -- make the key comparable
    comparableks = List.map f ks
  in
    -- make a dictionary now we have a comparable key
    Dict.fromList comparableks

{-| lookup a pitch class and see of it exists in the Accidentals set -}
lookup : PitchClass -> Accidentals -> Maybe Accidental
lookup pc accs =
  Dict.get (toString pc) accs

{- lookup a note and see if its pitch class exists in the Accidentals set -}
lookupNote : AbcNote -> Accidentals -> Maybe Accidental
lookupNote note =
  lookup note.pitchClass
