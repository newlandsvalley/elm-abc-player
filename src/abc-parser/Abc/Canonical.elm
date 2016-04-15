module Abc.Canonical 
   ( fromTune
   , fromResult
   ) where
   
{-|  Module for converting an ABC Tune parse tree to a canonical ABC string,
   
   # Definition
   
   # Functions
   @docs fromTune, fromResult
   
-}

import Abc.ParseTree exposing (..)
import Ratio exposing (Rational, numerator, denominator)
import Maybe exposing (withDefault)
import Music.Notation exposing (getKeySet, modifiedKeySet, naturaliseIfInKeySet, sharpenFlatEnharmonic)
import String exposing (fromChar, fromList, repeat, trimRight, toLower)

enquote : String -> String
enquote s = "\"" ++ s ++ "\""

mode : Mode -> String
mode m = case m of
  Major -> "major"
  Minor -> "minor"
  Ionian -> "ionian"
  Dorian -> "dorian"
  Phrygian -> "phrygian"
  Lydian -> "lydian"
  Mixolydian -> "mixolydian"
  Aeolian -> "aeolian"
  Locrian -> "locrian"
  
bar : Bar -> String
bar b = 
  let 
    it = withDefault "" (Maybe.map toString b.iteration)
    lines = String.repeat b.lines "|"
  in
    case b.repeat of
      Nothing -> 
        lines ++ it
      Just Begin -> 
        lines ++ ":"
      Just End -> 
        ":" ++ lines ++ it
      Just BeginAndEnd ->
        ":" ++ lines ++ ":"  
    
accidental : Accidental -> String
accidental a = case a of
  Sharp -> "^"
  Flat -> "_"
  DoubleSharp -> "^^"
  DoubleFlat -> "__"
  Natural -> "="
  
headerAccidental : Accidental -> String
headerAccidental a = case a of
  Sharp -> "#"
  Flat -> "b"
  _ -> ""
  
tuplet : TupletSignature -> String
tuplet t =
  let 
    (p,q,r) = t
  in
    case t of
      (2,3,2) -> "(2"
      (3,2,3) -> "(3"
      (4,3,4) -> "(4"
      _ ->  "(" 
         ++ (toString p)
         ++ ":"  
         ++ (toString q)
         ++ ":" 
         ++ (toString r)
    
tempo : TempoSignature -> String
tempo t =
  let
    text = withDefault "" (Maybe.map (\s -> " " ++ (enquote s)) t.marking)
    eq = if (List.length t.noteLengths == 0) then 
           ""
         else 
          "="
  in 
    ratlist t.noteLengths
        ++ eq
        ++ toString t.bpm
        ++ text

rational : Rational -> String
rational r =
  toString (numerator r) ++ "/" ++ toString (denominator r)

ratlist : List Rational -> String
ratlist rs = 
  let 
    f r acc = (rational r) ++ " " ++ acc
  in
    List.foldr f "" rs
      |> trimRight
    
meter : Maybe MeterSignature -> String
meter ms = 
  case ms of
    Nothing ->
      "none"
    Just m ->
      let 
        (n, d) = m
      in
        toString n ++ "/" ++ toString d

duration : NoteDuration -> String
duration nd = 
  if (denominator nd == 1) && (numerator nd == 1) then 
    ""
  else if (denominator nd == 2) && (numerator nd == 1) then 
    "/"
  else if (denominator nd == 1) then 
    toString (numerator nd)
  else 
    rational nd

key : KeySignature -> String
key k = 
   let
     acc = Maybe.map headerAccidental k.accidental
           |> withDefault ""
   in
     toString k.pitchClass ++ acc ++ toString k.mode

keyAccidental : KeyAccidental -> String
keyAccidental ka =
  let
    (pc, acc) = ka
  in
    accidental acc ++ toLower (toString pc)

keyAccidentals : List KeyAccidental -> String
keyAccidentals = 
  String.concat 
    << List.map (\a -> " " ++ keyAccidental a) 

octave : Int -> String
octave i =
  let
     octaveAboveMiddleC = middlecOctave + 1 
  in
     if ((i == middlecOctave) || (i == octaveAboveMiddleC)) then 
       ""
     else if (i > octaveAboveMiddleC) then 
       repeat (i - octaveAboveMiddleC) "'"
     else
       repeat (middlecOctave - i) ","      

pitch : Int -> PitchClass -> String
pitch octave p =
  if (octave <= middlecOctave) then
    toString p
  else
    toLower (toString p)   

abcNote : AbcNote -> KeySet -> String
abcNote originala ks =
  let
     -- forget about the accidental if it's implied by the key signature and otherwise sharoen flat accidentals
     a = sharpenFlatEnharmonic (naturaliseIfInKeySet originala ks)
     acc  = Maybe.map accidental a.accidental
                    |> withDefault ""
     tie = case a.tied of
       True -> "-"
       _ -> ""
  in
     acc
     ++ pitch a.octave a.pitchClass
     ++ octave a.octave
     ++ duration a.duration
     ++ tie

abcChord : AbcChord -> KeySet -> String
abcChord a ks =
     "[" ++ (notes a.notes ks) ++ "]"
       ++ duration a.duration
     
notes : List AbcNote -> KeySet -> String
notes ns ks = 
  let 
    f a acc = (abcNote a ks) ++ acc
  in
    List.foldr f "" ns   
     
rest : NoteDuration -> String
rest n =
  "z" ++ (duration n)

decorate : String -> String
decorate s =
  if (String.length s == 1) then 
    s
  else 
    "!" ++ s ++ "!"
    
musics : List Music-> KeySet -> String
musics ms ks = 
  let 
    f m acc = (music m ks) ++ acc
  in
    List.foldr f "" ms      

broken : Broken -> String
broken b = 
  case b of 
    LeftArrow i -> 
      String.repeat i "<"
    RightArrow i -> 
      String.repeat i ">"
     
music : Music -> KeySet -> String
music m ks = case m of
   Barline b -> bar b
   Note a -> abcNote a ks
   BrokenRhythmPair a1 b a2 -> abcNote a1 ks ++ (broken b) ++ abcNote a2 ks
   Rest r -> rest r
   Tuplet tup ns -> tuplet tup ++ notes ns ks
   Decoration s -> decorate s
   GraceNote isAcciaccatura m -> "{" ++ music m ks ++ "}"
   Slur c -> String.fromChar c
   Annotation placement s -> toString placement ++ ":" ++ s 
   ChordSymbol s -> enquote s
   Chord a -> abcChord a ks
   Inline h -> "[" ++ header h ++ "]"
   NoteSequence ms -> musics ms ks
   Spacer i -> " "
   Ignore -> ""
   Continuation -> "\\"
   -- _ -> ""

header : Header -> String
header h = case h of
   Area s -> "A: " ++ s
   Book s -> "B: " ++ s
   Composer s -> "C: " ++ s
   Discography s -> "D: " ++ s
   FileUrl s -> "F: " ++ s
   Group s -> "G: " ++ s
   History s -> "H: " ++ s
   Instruction s -> "I: " ++ s
   Key (k, kacc) -> "K: " ++ (key k) ++ (keyAccidentals kacc)
   UnitNoteLength d -> "L: " ++ (duration d)
   Meter m -> "M: " ++ (meter m)   
   Macro s -> "m: " ++ s
   Notes s -> "N: " ++ s
   Origin s -> "O: " ++ s
   Parts s -> "P: " ++ s
   Rhythm s -> "R: " ++ s
   Remark s -> "r: " ++ s
   Source s -> "S: " ++ s
   Title s -> "T: " ++ s
   Tempo t -> "Q: " ++ (tempo t) 
   UserDefined s -> "U: " ++ s
   Voice s -> "V: " ++ s
   WordsAfter s -> "W: " ++ s
   WordsAligned s -> "w: " ++ s
   ReferenceNumber i -> "X: " ++ (toString i)
   Transcription s -> "Z: " ++ s
   FieldContinuation s -> "+: " ++ s 
   Comment s -> "%" ++ s
   _ -> ""

tuneHeaders : List Header -> String
tuneHeaders  hs = 
  let 
    f h acc = (header h) ++ "\r\n" ++ acc
  in
    List.foldr f "" hs
    
{- need to return a KeySet along with the string to cover cases where an inline header changes key -}
bodyPart : BodyPart -> KeySet -> (String, KeySet)
bodyPart bp ks = case bp of
  Score ml -> (musics ml ks, ks)
  BodyInfo h ->  
    case h of
      -- an inline Key header indicating a key change so we must modify state
      Key mksig ->
        let
          ks1 = modifiedKeySet mksig
        in
          (header h, ks1)
      -- any other header has no effect on state
      _ -> (header h, ks)

continuation : Bool -> String
continuation c = 
  if c then 
    "\\"
  else 
    ""
{- Thread the KeySet state through each body part in order to cater for key changes introduced by inline Key headers -}
tuneBody : TuneBody -> KeySet -> String
tuneBody b ks = 
  let  
    f bp acc = 
      let
        (text0, ks0) = acc
        (text1, ks1) = (bodyPart bp ks0)
      in
        (text0 ++ text1 ++ "\r\n", ks1)
  in
    List.foldl f ("", ks) b
      |> fst

  
-- Exported Functions

{-| translate an ABC Tune parse tree to a canonical ABC String -}
fromTune : AbcTune -> String
fromTune t = 
  let
    ks = getKeySet t
  in
    tuneHeaders (fst t) ++ tuneBody (snd t) ks


{-| translate a parse Result containing an ABC Tune parse tree to a Result containing a canonical ABC String -}
fromResult : Result String AbcTune -> Result String String
fromResult r =
  Result.map fromTune r



