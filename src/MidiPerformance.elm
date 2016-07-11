module MidiPerformance exposing
  ( fromAbc
  , fromAbcResult
  , melodyFromAbc
  , melodyFromAbcResult
  , midiRecordingFromAbc
  ) 

{-|  conversion of a ABC Tune parse tree to a performance which is compatible with a MIDI recording

# Definition

# Functions
@docs fromAbc
    , fromAbcResult
    , melodyFromAbc
    , melodyFromAbcResult
    , midiRecordingFromAbc

-}

{- note on implementation 

   I originally folded from the right, which is conceptually much simpler and more efficient.
   However, I found that, because of the fact that an explicitly marked accidental influences
   notes of the same pitch class later on in the same bar, I was forced to fold from the left.
   This is more inefficient, as I have to reverse everything at the end.
-}

import Abc.ParseTree exposing (..)
import Abc exposing (ParseError)
import Music.Notation exposing (..)
import Music.Accidentals exposing (..)
import MidiMelody exposing (..)
import MidiTypes exposing (..)
import RepeatTypes exposing (..)
import Repeats exposing (..)
import String exposing (fromChar, toUpper)
import Ratio exposing (Rational, over, fromInt, toFloat, add, numerator, denominator)
import Maybe exposing (withDefault)

import Debug exposing (..)

type alias TranslationState = 
   { modifiedKeySignature : ModifiedKeySignature   -- the current key signature
   , tempo : AbcTempo                              -- the current tempo
   , tempoModifier : Rational                      -- a transient tempo modifier (e.g. in tuples or broken rhythm)
   , nextBarNumber : Int                           -- the number of the next bar to be added
   , thisBar : MidiBar                             -- the current bar being translated
   , lastNoteTied : Bool                           -- was the last note tied?
   , repeatState : RepeatState                     -- the repeat state of the tune
   }

-- default to 1/4=120
defaultTempo : AbcTempo
defaultTempo = 
  {  tempoNoteLength = over 1 4
  ,  bpm = 120
  ,  unitNoteLength = over 1 8
  }

-- default to C Major (i.e. no accidental modifiers)
defaultKey : ModifiedKeySignature
defaultKey = 
  (
    { pitchClass = C
    , accidental = Nothing
    , mode = Major
    }
  , [] 
  )
  
defaultBar : Int -> MidiBar
defaultBar i = 
  {  number = i
  ,  repeat = Nothing
  ,  iteration = Nothing
  ,  accidentals = Music.Accidentals.empty
  ,  notes = []
  }

isEmptyBar : MidiBar -> Bool
isEmptyBar b =
  List.length b.notes == 0

{- update the state of the player when we come across a header (either at the start or inline)
   which affects the tune tempo or the pitch of a note (i.e. they key)
-}
updateState :  Abc.ParseTree.Header -> (MidiMelody, TranslationState) -> (MidiMelody, TranslationState)
updateState h acc =
  let 
    (melody, state) = acc
    tempo = state.tempo
  in case h of
    UnitNoteLength d ->
      let
        newState = { state | tempo = { tempo | unitNoteLength = d }}
        mtempo = MTempo (midiTempo newState)
        newState1 = addTempoToStateBar mtempo newState
      in
        (melody, newState1)
    Abc.ParseTree.Tempo t ->
      let 
        tnl = List.foldl Ratio.add (fromInt 0) t.noteLengths
        newState = { state | tempo = { tempo | tempoNoteLength = tnl, bpm = t.bpm }}
        mtempo = MTempo (midiTempo newState)
        newState1 = addTempoToStateBar mtempo newState
      in
       (melody, newState1)
    -- ignore accidental note modifiers in key signatures for the moment - they're little used
    Key mk ->
       (melody, { state | modifiedKeySignature = mk} )
    _ -> acc       

{- work out the midiTempo in microseconds/Beat by investigating the state -}
midiTempo : TranslationState -> Int
midiTempo state =
  Music.Notation.midiTempo state.tempo

{- we need to take note of any accidentals so far in the bar because these may influence
   later notes in that bar.  Build the KeyAccidental for the accidental of the pitch class in question
   and add it to the list
-}
addNoteToBarAccidentals : MidiNote -> Accidentals -> Accidentals
addNoteToBarAccidentals n accs =
  case (n.pc, n.accidental) of
    (Just pitchClass, Just acc) ->
      Music.Accidentals.add pitchClass acc accs
    _ -> accs

{- ditto for midi note events (single notes or chords) -}
addMidiNoteToBarAccidentals :  MidiInstruction -> Accidentals -> Accidentals
addMidiNoteToBarAccidentals ne accs =
  case ne of
   MNote note _ -> addNoteToBarAccidentals note accs 
   MChord ns -> List.foldl (addNoteToBarAccidentals) accs ns
   MTempo _ -> accs

{- ditto for lists of note events -}
addMidiNotesToBarAccidentals :  List MidiInstruction -> Accidentals -> Accidentals
addMidiNotesToBarAccidentals nes accs =
  List.foldl (addMidiNoteToBarAccidentals) accs nes
    
{- add a midi note event to the state - add the note to the growing list of notes in the current bar
   and if the note has an explicit accidental marker, add it to the list of accidentals
-}
addNoteToState : MidiInstruction -> TranslationState -> TranslationState 
addNoteToState n state =
  let 
    line = state.thisBar.notes
    thisBar = state.thisBar
    accidentals = addMidiNoteToBarAccidentals n thisBar.accidentals
    lastNoteTied = 
      case n of
        MNote n tied -> tied
        _ -> False
  in
    { state | thisBar = { thisBar | notes = n :: line, accidentals = accidentals }
            , lastNoteTied = lastNoteTied
    }

{- add a midi tempo event to growing list of 'notes' in the current bar contained in the state -}
addTempoToStateBar : MidiInstruction -> TranslationState -> TranslationState 
addTempoToStateBar t state =
  let 
    line = state.thisBar.notes
    thisBar = state.thisBar
  in
    { state | thisBar = { thisBar | notes = t :: line  } }
  
  
{- ditto for a list of notes -}
addNotesToState : List MidiInstruction -> TranslationState -> TranslationState 
addNotesToState ns state =
  let 
    line = state.thisBar.notes
    thisBar = state.thisBar
    accidentals = addMidiNotesToBarAccidentals ns thisBar.accidentals
  in
    { state | thisBar = { thisBar | notes = List.append ns line, accidentals = accidentals }}

{- build a new bar from the bar number and the next ABC bar that we recognise.
   If the last bar was empty, retain its repeat markings, because otherwise we drop this bar
-}
buildNewBar : Int -> Bar -> MidiBar -> MidiBar
buildNewBar nextBarNumber abcBar lastBar =
  let
    nextBar = defaultBar nextBarNumber
  in
    if (isEmptyBar lastBar) then
      case (lastBar.repeat, abcBar.repeat) of
        (Just End, Just Begin) ->
          { nextBar | repeat = Just BeginAndEnd, iteration = abcBar.iteration }
        (Just x, _) ->
          { nextBar | repeat = Just x, iteration = abcBar.iteration }
        _ ->
          { nextBar | repeat = abcBar.repeat, iteration = abcBar.iteration }
    else
      { nextBar | repeat = abcBar.repeat, iteration = abcBar.iteration }


{- translate a sequence of notes as found in chords (parallel) or tuplets (sequential) -}
translateNoteSequence : Bool -> TranslationState -> List AbcNote -> Maybe NoteDuration -> List MidiInstruction
translateNoteSequence isSeq state notes maybeChordDur =
  let
    -- a chord can have a duration over and above that of any individual note in the chord
    chordDuration =
      case maybeChordDur of
        Nothing -> fromInt 1
        Just chordDur -> chordDur
    f abc = 
      let 
        ticks = 
          if (isSeq) then
            (noteTicks abc.duration * numerator state.tempoModifier) 
               // (denominator state.tempoModifier)
          else
            chordalNoteTicks abc.duration chordDuration
        barAccidentals = state.thisBar.accidentals
      in
        { ticks = ticks, pitch = toMidiPitch abc state.modifiedKeySignature barAccidentals, pc = Just abc.pitchClass, accidental = abc.accidental}
  in
    if isSeq then 
       List.map f notes
         |> List.map (\a -> MNote a False)
         |> List.reverse                     -- we need to reverse now we do a left fold
    else 
       [MChord (List.map f notes)]

{- translate a pair of notes, each working under a separate state -}
translateNotePair : AbcNote -> TranslationState -> AbcNote -> TranslationState -> List MidiInstruction
translateNotePair n1 s1 n2 s2  =
  let      
    -- modify the tempo according to the state
    ticks1 = (noteTicks n1.duration * numerator s1.tempoModifier) 
               // (denominator s1.tempoModifier)
    ticks2 = (noteTicks n2.duration * numerator s2.tempoModifier) 
               // (denominator s2.tempoModifier)

    -- but we'll just accumulate use accidentals in the first state (which will be identical to the second)
    barAccidentals = s1.thisBar.accidentals
    note1 = MNote { ticks = ticks1, pitch = toMidiPitch n1 s1.modifiedKeySignature barAccidentals, pc = Just n1.pitchClass, accidental = n1.accidental} False
    note2 = MNote { ticks = ticks2, pitch = toMidiPitch n2 s2.modifiedKeySignature barAccidentals, pc = Just n2.pitchClass, accidental = n2.accidental} False
  in
    -- we add them backwards now because we are doing a left fold
    [note2, note1]

{- not at all complete - translate Music items from the parse tree to a melody line - a sequence
   of bars containing notes, rests and chords where notes are in a MIDI-friendly format
-}
translateMusic : Music -> (MidiMelody, TranslationState) -> (MidiMelody, TranslationState)
translateMusic m acc =
  let 
    (midiMelody, state) = acc
  in
    case m of
      Note abc -> 
        let 
          ticks = (noteTicks abc.duration) 
          barAccidentals = state.thisBar.accidentals
          -- if the last note was tied, we treat this note simply as a rest (zero pitch) in order to pace the tune properly
          pitch = 
            if (state.lastNoteTied) then
              0
            else
              toMidiPitch abc state.modifiedKeySignature barAccidentals
          note = MNote { ticks = ticks, pitch = pitch, pc = Just abc.pitchClass, accidental = abc.accidental} abc.tied
          newState = addNoteToState note state
        in
          (midiMelody, newState)
      Rest r -> 
        let 
          ticks = (noteTicks r) 
          note = MNote { ticks = ticks, pitch = 0, pc = Nothing, accidental = Nothing } False
          newState = addNoteToState note state
        in 
          (midiMelody, newState)
      Tuplet signature tnotes ->
        let 
          (p,q,r) = signature
          tupletState = { state | tempoModifier = q `over` p }
          tupletNotes = translateNoteSequence True tupletState tnotes Nothing
          newState = addNotesToState tupletNotes state
        in 
          (midiMelody, newState)
      BrokenRhythmPair n1 b n2 ->  
        case b of 
          LeftArrow i ->
            let
              down = Ratio.add (1 `over` 1) (Ratio.negate (dotFactor i))
              up = Ratio.add (1 `over` 1) (dotFactor i)
              leftState =  { state | tempoModifier = down }
              rightState =  { state | tempoModifier = up }
              notePair = translateNotePair n1 leftState n2 rightState
              newState = addNotesToState notePair state
            in              
              (midiMelody, newState)
          RightArrow i ->
            let
              down = Ratio.add (1 `over` 1) (Ratio.negate (dotFactor i))
              up = Ratio.add (1 `over` 1) (dotFactor i)
              leftState =  { state | tempoModifier = up }
              rightState =  { state | tempoModifier = down }
              notePair = translateNotePair n1 leftState n2 rightState
              newState = addNotesToState notePair state
            in              
              (midiMelody, newState)
      Chord abcChord ->
        let 
          chord = translateNoteSequence False state abcChord.notes (Just abcChord.duration)
          newState = addNotesToState chord state
        in             
          (midiMelody, newState)
      Barline b ->
        let 
          -- don't add to the melody the existing bar accumulated by the state if it's empty
          newMelody = 
            if (isEmptyBar state.thisBar) then
              midiMelody
            else
              let
                rb = state.thisBar
              in
                state.thisBar :: midiMelody
          -- don't increment the bar number if it's an empty bar
          nextBarNumber =   
            if (isEmptyBar state.thisBar) then
               state.nextBarNumber
            else
               state.nextBarNumber + 1

          {-
          nextBar = defaultBar nextBarNumber
          newBar = { nextBar | repeat = b.repeat, iteration = b.iteration }
          -}
          -- build a new Bar from the incoming AbcBar, retaining any unused state from the last bar if it was empty (and hence to be dropped)
          newBar = buildNewBar nextBarNumber b state.thisBar
          -- index the last bar if it was not empty   
          repeatState = 
            if (isEmptyBar state.thisBar) then
              state.repeatState
            else
              indexBar state.thisBar state.repeatState
          newState =  { state | thisBar = newBar, nextBarNumber = nextBarNumber, repeatState = repeatState }
        in
          (newMelody, newState)
      Inline header ->
        updateState header acc
      _ -> acc

-- translate an entire melody line from the tune body (up to an end of line)
toMidiMelody : MusicLine -> (MidiMelody, TranslationState) -> (MidiMelody, TranslationState)
toMidiMelody ml state =
  let 
    (melody, s) = List.foldl translateMusic state ml
  in
    (melody, s)
  
reverseMelody : MidiMelody -> MidiMelody
reverseMelody =
  let 
    reverseBar b = { b | notes = List.reverse b.notes }
  in
    List.map reverseBar
     >> List.reverse
 
{- translate an AbcTune to a more playable melody line
   which is a list of notes (or rests) and their durations
-}
fromAbc : AbcTune -> (MidiMelody, Repeats)
fromAbc tune =   
  let
    -- set a default state for case where there are no tune headers
    defaultState = ([], { modifiedKeySignature = defaultKey
                        , tempo = defaultTempo
                        , tempoModifier = 1 `over` 1
                        , nextBarNumber = 0
                        , thisBar = defaultBar 0
                        , lastNoteTied = False
                        , repeatState = defaultRepeatState
                        }
                    )

    abcHeaders = fst tune    
    {- if there is no Tempo indication of any kind in the ABC headers, then add one.
       This ensures a default Tempo 'note' message will be placed at the start of the MIDI track
    -}
    initialHeaders =
      if hasTempoHeader abcHeaders then
        abcHeaders
      else
        UnitNoteLength defaultTempo.unitNoteLength :: abcHeaders

    -- update this from the header state if we have any headers
    headerState = List.foldl updateState defaultState initialHeaders
    f bp acc = case bp of
      -- process a line from the melody using the current state
      Score musicLine -> 
        let 
          (existingLine, state) = acc
          (newLine, newState) = toMidiMelody musicLine acc
        in
          (newLine, newState)
      -- update the state if we have an inline header
      BodyInfo header -> 
        updateState header acc
   in 
     let
        (music, state) =  List.foldl f headerState (snd tune)
        -- ensure we don't forget the residual closing bar (still kept in the state) which may yet contain music
        fullMusic =            
           reverseMelody (state.thisBar :: music)
        -- finalise the repeat state with the last bar
        repeatState = finalise state.thisBar state.repeatState
        -- _ = log "repeats" (List.reverse repeatState.repeats)
     in
       (fullMusic, (List.reverse repeatState.repeats))

{- does the tune contain any tempo-related ABC header? -}
hasTempoHeader : TuneHeaders -> Bool
hasTempoHeader hs =
  let
    f h =
      case h of
        UnitNoteLength _ -> True
        Abc.ParseTree.Tempo _ -> True
        _ -> False
    tempoHeaders = List.filter f hs
  in
    (List.length tempoHeaders) > 0

melodyFromAbc : Bool -> AbcTune -> (MidiMelody, Repeats)
melodyFromAbc expandRepeats tune =
  let
    mr = fromAbc tune
  in
    if (expandRepeats) then
      (buildRepeatedMelody mr, [])
    else
      mr

{- just a NoteOn - for use in chords -}
midiNoteOn : MidiNote -> MidiMessage
midiNoteOn n =
  (0, NoteOn 0 n.pitch 63)

{- just a NoteOff - for use after the final note in chords -}
midiNoteOff : MidiNote -> MidiMessage
midiNoteOff n =
  (n.ticks, NoteOff 0 n.pitch 63)

{- if the note has a pitch, issue a NoteOn/NoteOff pair otherwise
   pace the rest by issuing an arbitrary text instruction with the
   correct timing
-}
midiNote : Bool -> MidiNote -> List MidiMessage
midiNote isTied n =
  if (n.pitch > 0) then
    [  (0, NoteOn 0 n.pitch 63)
    ,  (n.ticks, NoteOff 0 n.pitch 63)
    ]
  else
    [ (n.ticks, Text "rest") ]

{- play a MIDI chord - all notes in the chord start at time 0 and then
   we pace the tune properly by just switching the first note off
-}
midiChord : List MidiNote -> List MidiMessage
midiChord ns =
  let 
    firstNote = List.head ns
    finalNoteOff =
      case firstNote of
        Just n -> [midiNoteOff n]
        _ -> []
  in
    List.map (midiNoteOn) ns
      ++ finalNoteOff
    

toMidiMessages : MidiInstruction -> List MidiMessage
toMidiMessages mi =
  case mi of
     MNote note isTied  ->
       midiNote isTied note 
     MChord notes ->
       midiChord notes
     MTempo t ->
       [(0, MidiTypes.Tempo t)]

midiBar : MidiBar -> List MidiMessage
midiBar b =
  List.map toMidiMessages b.notes
    |> List.concat

toMidiTrack : MidiMelody -> Track
toMidiTrack melody =
  -- [(0, TrackName "test")]
  List.map midiBar melody
    |> List.concat

{-| translate an ABC tune into a MIDI recording -}
midiRecordingFromAbc : Bool -> AbcTune -> MidiRecording
midiRecordingFromAbc expandRepeats tune =
  let
    -- _ = log "abc tune" tune
    (melody, _) = melodyFromAbc expandRepeats tune
    -- _ = log "melody" melody
    track = toMidiTrack melody
    -- _ = log "track" track
    header =    
     { formatType = 0
     , trackCount = 1
     , ticksPerBeat = standardMidiTick
     }
  in
    (header, [track])

  

fromAbcResult : Result ParseError AbcTune -> Result ParseError (MidiMelody, Repeats)
fromAbcResult r =
  Result.map fromAbc r

melodyFromAbcResult : Result ParseError AbcTune -> Result ParseError MidiMelody
melodyFromAbcResult r =
  -- Result.map (fromAbc >> fst) r
  Result.map (fromAbc >> buildRepeatedMelody) r




