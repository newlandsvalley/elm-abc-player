module Test.MidiPerformance exposing 
  (tests) 

import Test exposing (..)
import Expect exposing (..)
import Abc exposing (ParseError, parse, parseKeySignature, parseError)
import Abc.ParseTree exposing (..)
import MidiPerformance exposing (..)
import Music.Notation exposing (NoteTime, MidiPitch)
import MidiMelody exposing (..)
import RepeatTypes exposing (..)
import Maybe exposing (withDefault)

import Debug exposing (..)

{- get the melody line -}
getMidiMelody : String -> Result ParseError MidiMelody
getMidiMelody s = 
  parse s
     |> melodyFromAbcResult

{- get notes from the second bar 
   (the first bar just holds the MIDI tempo)
-}
getSecondBarNotes : String -> List MidiInstruction
getSecondBarNotes s =
  let 
    melodyResult = getMidiMelody s
  in 
    case melodyResult of
      Ok res -> 
        let 
          tail = List.tail res
                   |> withDefault []
          maybeSecondBar = List.head tail         
        in 
          case maybeSecondBar of
            Just abar -> abar.notes
            _ -> []
      Err errs -> 
        []


{- show bar 2 and assert we have some notes -}
showBar2Notes : String -> Expectation
showBar2Notes s = 
  let 
    bar2Notes = getSecondBarNotes s
  in 
    Expect.false "non empty bar" (List.isEmpty bar2Notes)

{- assert the notes match the target from bar 2 in the melody line -}
assertBar2Notes : String -> List MidiInstruction -> Expectation
assertBar2Notes s target = 
  Expect.equal target (getSecondBarNotes s)


tests : Test
tests = 
  describe "MIDI Performance melody line"
        [ test "sequence" <|
            \() -> (assertBar2Notes sequence sequenceM)
        , test "chord" <|
            \() -> (assertBar2Notes chord chordM)
        , test "triplet" <|
            \() -> (assertBar2Notes triplet tripletM)
        , test "broken rhythm up" <| 
            \() -> (assertBar2Notes brokenRhythmUp brokenRhythmUpM)
        , test "broken rhythm down" <|
            \() -> (assertBar2Notes brokenRhythmDown brokenRhythmDownM)
        , test "tied sequence" <|
            \() -> (assertBar2Notes sequenceTied sequenceTiedM)
        , test "sequence with accidental" <|
            \() -> (assertBar2Notes sequenceAccidental sequenceAccidentalM)
        , test "triplet with accidental" <|
            \() -> (assertBar2Notes tripletAccidental tripletAccidentalM)
        , test "broken rhythm with accidental" <|
            \() -> (assertBar2Notes brokenRhythmAccidental brokenRhythmAccidentalM)
        , test "broken rhythm with internal accidental" <|
            \() -> (assertBar2Notes brokenRhythmInternalAccidental brokenRhythmInternalAccidentalM)
        ]

-- each test should just investigate a single bar

-- music sources
sequence = "K: D\r\n| def |\r\n"
chord = "K: D\r\n| [def]2 |\r\n"
triplet = "K: D\r\n| (3def |\r\n"
brokenRhythmUp = "K: D\r\n| d<e e |\r\n"
brokenRhythmDown = "K: D\r\n| d>e e |\r\n"
sequenceTied = "K: D\r\n| def- fg |\r\n"
sequenceAccidental = "K: D\r\n| =c2d2c2 |\r\n"  -- second c inherits natural from first 
tripletAccidental = "K: D\r\n| (3=c2d2c2 |\r\n"  -- second c inherits natural from first 
brokenRhythmAccidental = "K: D\r\n| d>=c c |\r\n" -- second c inherits natural from first 
brokenRhythmInternalAccidental = "K: D\r\n| =c>c c |\r\n" -- last 2 cs inherit natural from first 

{- target melodies
   in all these samples, pc and accidental are merely for debugging purposes - the note itself
   is defined by the ticks and the pitch (and, I suppose the Boolean indicating if it is tied)
-}
sequenceM = 
  [ MNote { ticks = 480, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks = 480, pitch = 76, pc = Just E, accidental = Nothing } False
  , MNote { ticks = 480, pitch = 78, pc = Just F, accidental = Nothing } False
  ]
chordM = 
  [MChord (
    [ { ticks = 960, pitch = 74, pc = Just D, accidental = Nothing }
    , { ticks = 960, pitch = 76, pc = Just E, accidental = Nothing }
    , { ticks = 960, pitch = 78, pc = Just F, accidental = Nothing }
    ]
  )]
tripletM =  
  [ MNote { ticks =  320, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks =  320, pitch = 76, pc = Just E, accidental = Nothing } False
  , MNote { ticks =  320, pitch = 78, pc = Just F, accidental = Nothing } False
  ]
brokenRhythmUpM =
  [ MNote { ticks = 240, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks = 720, pitch = 76, pc = Just E, accidental = Nothing } False
  , MNote { ticks = 480, pitch = 76, pc = Just E, accidental = Nothing } False
  ]
brokenRhythmDownM =
  [ MNote { ticks = 720, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks = 240, pitch = 76, pc = Just E, accidental = Nothing } False
  , MNote { ticks = 480, pitch = 76, pc = Just E, accidental = Nothing } False
  ]
sequenceTiedM =  
  [ MNote { ticks = 480, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks = 480, pitch = 76, pc = Just E, accidental = Nothing } False
  , MNote { ticks = 480, pitch = 78, pc = Just F, accidental = Nothing } True
  , MNote { ticks = 480, pitch = 0, pc = Just F, accidental = Nothing } False  -- pitch is 0 cos this is how we implement ties
  , MNote { ticks = 480, pitch = 79, pc = Just G, accidental = Nothing } False
  ]
sequenceAccidentalM =
  [ MNote { ticks = 960, pitch = 72, pc = Just C, accidental = Just Natural } False
  , MNote { ticks = 960, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks = 960, pitch = 72, pc = Just C, accidental = Nothing } False
  ]
tripletAccidentalM =
  [ MNote { ticks = 640, pitch = 72, pc = Just C, accidental = Just Natural } False
  , MNote { ticks = 640, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks = 640, pitch = 72, pc = Just C, accidental = Nothing } False
  ]
brokenRhythmAccidentalM =
  [ MNote { ticks = 720, pitch = 74, pc = Just D, accidental = Nothing } False
  , MNote { ticks = 240, pitch = 72, pc = Just C, accidental = Just Natural } False
  , MNote { ticks = 480, pitch = 72, pc = Just C, accidental = Nothing } False
  ]
brokenRhythmInternalAccidentalM =
  [ MNote { ticks = 720, pitch = 72, pc = Just C, accidental = Just Natural } False
  , MNote { ticks = 240, pitch = 72, pc = Just C, accidental = Nothing } False
  , MNote { ticks = 480, pitch = 72, pc = Just C, accidental = Nothing } False
  ]



