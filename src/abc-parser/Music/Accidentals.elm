module Music.Accidentals
    exposing
        ( Accidentals
        , empty
        , add
        , fromKeySet
        , lookup
        , member
        )

{-| private module to provide a type-safe interface into handling
    sets of Key Accidentals - i.e. pitch classes associated
    with an explicit sharp, flat or natural modifier.

    (as of elm 0.16, type-safety of Dicts is compromised)
-}

import Abc.ParseTree exposing (AbcNote, PitchClass(..), Accidental(..), KeyAccidental, KeySet)
import Dict exposing (Dict, fromList, get)
import Debug exposing (..)


{-| A set of accidentals,  String is a proxy for PitchClass
-}
type alias Accidentals =
    Dict String Accidental


{-| create an empty set of Key Accidentals
-}
empty : Accidentals
empty =
    Dict.empty


{-| add an accidental to the set
-}
add : PitchClass -> Accidental -> Accidentals -> Accidentals
add pc acc accs =
    Dict.insert (toString pc) acc accs


{-| build Accidentals from a KeySet
-}
fromKeySet : KeySet -> Accidentals
fromKeySet ks =
    let
        f ( pc, acc ) =
            ( toString pc, acc )

        -- make the key comparable
        comparableks =
            List.map f ks
    in
        -- make a dictionary now we have a comparable key
        Dict.fromList comparableks


{-| lookup a pitch class and see of it exists in the Accidentals set
-}
lookup : PitchClass -> Accidentals -> Maybe Accidental
lookup pc accs =
    Dict.get (toString pc) accs



{-
   let
     _ = log "lookup" pc
     _ = log "in" accs
     res = log "result" (Dict.get (toString pc) accs)
   in
     res
-}


{-| lookup a KeyAccidental and see if it's a member of the Accidentals set
    (i.e. the value of the Accidental matches for the supplied pitch)
-}
member : KeyAccidental -> Accidentals -> Bool
member ka accs =
    let
        ( pc, acc ) =
            ka

        macc =
            Dict.get (toString pc) accs
    in
        (Just acc) == macc
