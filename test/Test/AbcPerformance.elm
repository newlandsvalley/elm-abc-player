module Test.AbcPerformance exposing 
  (tests) 

import ElmTest exposing (..)
import Abc exposing (ParseError, parse, parseKeySignature, parseError)
import Abc.ParseTree exposing (..)
import AbcPerformance exposing (..)
import Music.Notation exposing (NoteTime, MidiPitch)
import Melody exposing (..)
import RepeatTypes exposing (..)

import Debug exposing (..)



{- get the melody line -}
getMelodyLine : String -> Result ParseError MelodyLine
getMelodyLine s = 
  parse s
     |> melodyFromAbcResult

{- get notes from the first bar -}
getFirstBarNotes : String -> List NoteEvent
getFirstBarNotes s =
  let 
    melodyResult = getMelodyLine s
  in 
    case melodyResult of
      Ok res -> 
        let 
          maybeFirstBar = List.head res
        in 
          case maybeFirstBar of
            Just abar -> abar.notes
            _ -> []
      Err errs -> 
        []


{- show bar 1 and assert we have some notes -}
showBar1Notes : String -> Assertion
showBar1Notes s = 
  let 
    bar1Notes = getFirstBarNotes s
  in 
    if List.isEmpty bar1Notes then
      assert False
    else
      let
         _ = log "bar 1 notes" bar1Notes
      in
        assert True 

{- assert the notes match the target from bar 1 in the melody line -}
assertBar1Notes : String -> List NoteEvent -> Assertion
assertBar1Notes s target = 
  assertEqual target (getFirstBarNotes s)


tests : Test
tests =
  let 
    melodyLine = 
      suite "melody line"
        [ test "sequence" (assertBar1Notes sequence sequenceM)
        , test "chord" (assertBar1Notes chord chordM)
        , test "triplet" (assertBar1Notes triplet tripletM)
        , test "broken rhythm up" (assertBar1Notes brokenRhythmUp brokenRhythmUpM)
        , test "broken rhythm down" (assertBar1Notes brokenRhythmDown brokenRhythmDownM)
        , test "tied sequence" (assertBar1Notes sequenceTied sequenceTiedM)
        , test "sequence with accidental" (assertBar1Notes sequenceAccidental sequenceAccidentalM)
        , test "triplet with accidental" (assertBar1Notes tripletAccidental tripletAccidentalM)
        , test "broken rhythm with accidental" (assertBar1Notes brokenRhythmAccidental brokenRhythmAccidentalM)
        , test "broken rhythm with internal accidental" (assertBar1Notes brokenRhythmInternalAccidental brokenRhythmInternalAccidentalM)
        ]
  in
    suite "ABC Performance"
      [  melodyLine
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
   is defined by the time and the pitch (and, I suppose the Boolean indicating if it is tied)
-}
sequenceM = 
  [ ANote { time = 0.25, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 76, pc = Just E, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 78, pc = Just F, accidental = Nothing } False
  ]
chordM = 
  [AChord (
    [ { time = 0.5, pitch = 74, pc = Just D, accidental = Nothing }
    , { time = 0.5, pitch = 76, pc = Just E, accidental = Nothing }
    , { time = 0.5, pitch = 78, pc = Just F, accidental = Nothing }
    ]
  )]
tripletM =  
  [ ANote { time =  0.16666666666666666, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.16666666666666666, pitch = 76, pc = Just E, accidental = Nothing } False
  , ANote { time = 0.16666666666666666, pitch = 78, pc = Just F, accidental = Nothing } False
  ]
brokenRhythmUpM =
  [ ANote { time = 0.125, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.375, pitch = 76, pc = Just E, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 76, pc = Just E, accidental = Nothing } False
  ]
brokenRhythmDownM =
  [ ANote { time = 0.375, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.125, pitch = 76, pc = Just E, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 76, pc = Just E, accidental = Nothing } False
  ]
sequenceTiedM =  
  [ ANote { time = 0.25, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 76, pc = Just E, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 78, pc = Just F, accidental = Nothing } True
  , ANote { time = 0.25, pitch = 78, pc = Just F, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 79, pc = Just G, accidental = Nothing } False
  ]
sequenceAccidentalM =
  [ ANote { time = 0.5, pitch = 72, pc = Just C, accidental = Just Natural } False
  , ANote { time = 0.5, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.5, pitch = 72, pc = Just C, accidental = Nothing } False
  ]
tripletAccidentalM =
  [ ANote { time = 0.3333333333333333, pitch = 72, pc = Just C, accidental = Just Natural } False
  , ANote { time = 0.3333333333333333, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.3333333333333333, pitch = 72, pc = Just C, accidental = Nothing } False
  ]
brokenRhythmAccidentalM =
  [ ANote { time = 0.375, pitch = 74, pc = Just D, accidental = Nothing } False
  , ANote { time = 0.125, pitch = 72, pc = Just C, accidental = Just Natural } False
  , ANote { time = 0.25, pitch = 72, pc = Just C, accidental = Nothing } False
  ]
brokenRhythmInternalAccidentalM =
  [ ANote { time = 0.375, pitch = 72, pc = Just C, accidental = Just Natural } False
  , ANote { time = 0.125, pitch = 72, pc = Just C, accidental = Nothing } False
  , ANote { time = 0.25, pitch = 72, pc = Just C, accidental = Nothing } False
  ]




