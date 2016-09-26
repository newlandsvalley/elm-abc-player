module Music.Transposition
    exposing
        ( keyDistance
        , transposeNote
        , transposeTo
        )

{-| Experimental Module for tune transposition

You can transpose a parsed ABC tune (in whatever key - perhaps in C) to (say) G# using:

    transposedResult = formatError (\_ -> "parse error") (parse source)
        `andThen` (\tune -> transposeTo ({pitchClass = G, accidental = Just Sharp, mode = Major},[]) tune)


The mode before and after transposition must be identical (that is: you cannot transpose minor to major and so on).
Chord symbols will be lost on transposition.

# Definition

# Functions
@docs keyDistance
    , transposeNote
    , transposeTo


-}

{- A parse tree score contains implicit accidentals.  Very often, the source text will not mark them but assume that they
   are implicit in the key signature.  They also may appear 'locally' - i.e. earlier in the bar and thus inherited.
   In order for the transposition process to work, all accidentals must be made explicit during transposition and then
   made implicit when written out to text if they are defined earlier in the bar or are defined in the key signature.

   This means we have to thread state through the transposition and hence use folds rather than maps
-}

import Dict exposing (Dict, fromList, get)
import Maybe exposing (withDefault, oneOf)
import Maybe.Extra exposing (isJust)
import Abc.ParseTree exposing (..)
import Music.Notation exposing (..)
import Music.Accidentals exposing (..)
import Debug exposing (..)


type alias TranspositionState =
    { keyDistance :
        Int
        -- semitone distance between keys - may be positive or negative
    , sourcemks :
        ModifiedKeySignature
        -- source key signature
    , sourceBarAccidentals :
        Accidentals
        -- any accidental defined locally to the current bar in the tune source
    , targetmks :
        ModifiedKeySignature
        -- target key signature
    , targetKeySet :
        KeySet
        -- the set of accidental keys in the target key signature
    , targetScale :
        DiatonicScale
        -- diatonic scale in the target key
    , targetBarAccidentals :
        Accidentals
        -- any accidental defined locally to the current bar in the tune target
    }



-- Exposed API


{-| work out the distance between the keys (target - source) measured in semitones.
   Keys must be in compatible modes
-}
keyDistance : ModifiedKeySignature -> ModifiedKeySignature -> Result String Int
keyDistance targetmks srcmks =
    let
        target =
            fst targetmks

        src =
            fst srcmks
    in
        if (target.mode /= src.mode) then
            Err "incompatible modes"
        else
            Ok (transpositionDistance ( target.pitchClass, target.accidental ) ( src.pitchClass, src.accidental ))


{-| transpose a note from its source key to its target
-}
transposeNote : ModifiedKeySignature -> ModifiedKeySignature -> AbcNote -> Result String AbcNote
transposeNote targetmks srcKey note =
    let
        rdist =
            keyDistance targetmks srcKey
    in
        case rdist of
            Err e ->
                Err e

            Ok d ->
                let
                    transpositionState =
                        { keyDistance = d
                        , sourcemks = srcKey
                        , sourceBarAccidentals = Music.Accidentals.empty
                        , targetmks = targetmks
                        , targetKeySet = modifiedKeySet targetmks
                        , targetScale = diatonicScale (fst targetmks)
                        , targetBarAccidentals = Music.Accidentals.empty
                        }

                    ( transposedNote, _ ) =
                        (transposeNoteBy transpositionState note)
                in
                    Ok transposedNote


{-| transpose a tune to the target key
-}
transposeTo : ModifiedKeySignature -> AbcTune -> Result String AbcTune
transposeTo targetmks t =
    let
        -- get the key signature if there is one, default to C Major
        mks =
            getKeySig t
                |> withDefault ( { pitchClass = C, accidental = Nothing, mode = Major }, [] )

        -- find the distance between the keys
        rdistance =
            keyDistance targetmks mks
    in
        case rdistance of
            Err e ->
                Err e

            Ok d ->
                -- don't bother transposing if there's no distance between the keys
                if (d == 0) then
                    Ok t
                else
                    let
                        transpositionState =
                            { keyDistance = d
                            , sourcemks = mks
                            , sourceBarAccidentals = Music.Accidentals.empty
                            , targetmks = targetmks
                            , targetKeySet = modifiedKeySet targetmks
                            , targetScale = diatonicScale (fst targetmks)
                            , targetBarAccidentals = Music.Accidentals.empty
                            }
                    in
                        Ok (transposeTune transpositionState t)



-- Implementation


transposeTune : TranspositionState -> AbcTune -> AbcTune
transposeTune state t =
    let
        ( headers, body ) =
            t

        newHeaders =
            replaceKeyHeader state.targetmks headers
    in
        ( newHeaders, (transposeTuneBody state body) )



{- transpose the tune body.  We need to thread state through the tune in case there's an inline
   information header which changes key part way through the tune
-}


transposeTuneBody : TranspositionState -> TuneBody -> TuneBody
transposeTuneBody state body =
    let
        f n acc =
            let
                ( bs, s0 ) =
                    acc

                ( b1, s1 ) =
                    transposeBodyPart s0 n
            in
                ( b1 :: bs, s1 )
    in
        let
            ( tb, news ) =
                List.foldl f ( [], state ) body
        in
            List.reverse tb


processHeader : TranspositionState -> Header -> ( Header, TranspositionState )
processHeader state h =
    case h of
        Key mks ->
            let
                newmks =
                    transposeKeySignatureBy state.keyDistance mks

                -- newmks = log "\r\nkey change\r\n" (transposeKeySignatureBy state.keyDistance mks)
            in
                ( Key newmks
                , { state
                    | sourcemks = mks
                    , sourceBarAccidentals = Music.Accidentals.empty
                    , targetmks = newmks
                    , targetKeySet = modifiedKeySet newmks
                    , targetScale = diatonicScale (fst newmks)
                    , targetBarAccidentals = Music.Accidentals.empty
                  }
                )

        _ ->
            ( h, state )


transposeBodyPart : TranspositionState -> BodyPart -> ( BodyPart, TranspositionState )
transposeBodyPart state bp =
    case bp of
        -- just transpose the score
        Score ms ->
            let
                ( ms1, s1 ) =
                    transposeMusicList state ms
            in
                ( Score ms1, s1 )

        -- transpose any Key header found inline
        BodyInfo h ->
            let
                ( h1, state ) =
                    processHeader state h
            in
                ( BodyInfo h1, state )


transposeMusic : TranspositionState -> Music -> ( Music, TranspositionState )
transposeMusic state m =
    case m of
        Note n ->
            let
                ( tn1, s1 ) =
                    transposeNoteBy state n
            in
                ( Note tn1, s1 )

        BrokenRhythmPair n1 b n2 ->
            let
                ( tn1, s1 ) =
                    transposeNoteBy state n1

                ( tn2, s2 ) =
                    transposeNoteBy s1 n2
            in
                ( BrokenRhythmPair tn1 b tn2, s2 )

        Tuplet ts ns ->
            let
                ( ns1, s1 ) =
                    transposeNoteList state ns
            in
                ( Tuplet ts ns1, s1 )

        GraceNote b ns ->
            let
                ( m1, s1 ) =
                    transposeNoteList state ns
            in
                ( GraceNote b m1, s1 )

        Chord c ->
            let
                ( tc, s1 ) =
                    transposeChord state c
            in
                ( Chord tc, s1 )

        -- we won't attempt to transpose chord symbols - just quietly drop them
        ChordSymbol s ->
            ( Ignore, state )

        -- new bar, initialise accidentals list
        Barline b ->
            ( Barline b, { state | sourceBarAccidentals = Music.Accidentals.empty, targetBarAccidentals = Music.Accidentals.empty } )

        -- an inline header
        Inline h ->
            let
                ( h1, state ) =
                    processHeader state h
            in
                ( Inline h1, state )

        _ ->
            ( m, state )


transposeMusicList : TranspositionState -> List Music -> ( List Music, TranspositionState )
transposeMusicList state ms =
    let
        f n acc =
            let
                ( ns, s0 ) =
                    acc

                ( n1, s1 ) =
                    transposeMusic s0 n
            in
                ( n1 :: ns, s1 )
    in
        let
            ( tns, news ) =
                List.foldl f ( [], state ) ms
        in
            ( List.reverse tns, news )


transposeNoteList : TranspositionState -> List AbcNote -> ( List AbcNote, TranspositionState )
transposeNoteList state ns =
    let
        f n acc =
            let
                ( ns, s0 ) =
                    acc

                ( n1, s1 ) =
                    transposeNoteBy s0 n
            in
                ( n1 :: ns, s1 )
    in
        let
            ( tns, news ) =
                List.foldl f ( [], state ) ns
        in
            ( List.reverse tns, news )


transposeChord : TranspositionState -> AbcChord -> ( AbcChord, TranspositionState )
transposeChord state c =
    let
        ( ns, newstate ) =
            transposeNoteList state c.notes
    in
        ( { c | notes = ns }, newstate )


{-| transpose a note by the required distance which may be positive or negative
    transposition distance and source and target keys are taken from the state.  This is the heart of the module.

    The strategy is:
      * does the source note have an explicit accidental?
      * if not, does it have an implicit accidental?  i.e. implied firstly by an earlier explicit in the bar or secondly by the key signature
      * move the note to the target by the required distance between the keys, using either form of accidental if present
      * use a convention for the target note that if it has an explicit accidental, the accidental in the full note is represented as Just Acc - otherwise None
      * to decide on whether an explicit accidental is present, first look up the exact note in the target bar accidentals, then do so for just the pitch
        (i.e. there is an accidental for the pitch but it's a different one) and finally look up the note in the target scale for the key in question
      * if there is an explicit accidental, save the source note in the source bar accidentals, ditto for the target, keep in the state
-}
transposeNoteBy : TranspositionState -> AbcNote -> ( AbcNote, TranspositionState )
transposeNoteBy state note =
    let
        -- make any implicit accidental explicit in the source note to be transposed if it's not marked as an accidental
        inSourceKeyAccidental =
            accidentalImplicitInKey note.pitchClass (state.sourcemks)

        inSourceBarAccidental =
            lookup note.pitchClass state.sourceBarAccidentals

        -- we must do the lookup of the source accidental in this order - local bar overrides key
        implicitSourceAccidental =
            oneOf [ inSourceBarAccidental, inSourceKeyAccidental ]

        explicitSourceNote =
            if (isJust note.accidental) then
                note
            else
                { note | accidental = implicitSourceAccidental }

        srcNum =
            noteNumber explicitSourceNote

        ( targetNum, octaveIncrement ) =
            noteIndex srcNum (state.keyDistance)

        ka =
            pitchFromInt (fst state.targetmks) targetNum

        ( pc, acc ) =
            sharpenFlatEnharmonic ka

        targetAcc =
            -- is it present in the local target bar accidentals
            if (Music.Accidentals.member ( pc, acc ) state.targetBarAccidentals) then
                Nothing
                -- is it present in the local target bar accidentals but with a different value
            else if (isJust (Music.Accidentals.lookup pc state.targetBarAccidentals)) then
                Just acc
                -- is it in the set of keys in the target diatonic scale
            else if (inScale ( pc, acc ) state.targetScale) then
                Nothing
            else
                Just acc

        transposedNote =
            { note | pitchClass = pc, accidental = targetAcc, octave = note.octave + octaveIncrement }

        {-
           _ = log "\r\nsource note" note
           _ = log "source note made explicit" explicitSourceNote
           _ = log "source accidentals" state.sourceBarAccidentals
           _ = log "target keyset" state.targetKeySet
           _ = log "target key sig " state.targetmks
           _ = log "target accidentals" state.targetBarAccidentals
           _ = log "target scale" state.targetScale
           _ = log "transposed note" transposedNote
           _ = log "accidentalised target note" (pc, acc)
           _ = log "is transposed note a member of target accs" (Music.Accidentals.member (pc, acc) state.targetBarAccidentals)
        -}
        -- save any accidental nature of the original untransposed note
        newSourceAccs =
            addBarAccidental note.pitchClass note.accidental state.sourceBarAccidentals

        -- if the target, after all this, has an explicit accidental, we need to save it in the target bar accidental state
        newTargetAccs =
            if (isJust targetAcc) then
                -- we use the explicit form of the target
                addBarAccidental pc (Just acc) state.targetBarAccidentals
            else
                state.targetBarAccidentals

        -- update the state with both the source an target bar accidentals
        newState =
            { state | sourceBarAccidentals = newSourceAccs, targetBarAccidentals = newTargetAccs }
    in
        ( transposedNote, newState )


{-| enharmonic equivalence for flattened accidentals
   We'll (for the moment) use the convention that most flat accidentals
   are presented in sharpened form
-}
sharpenFlatEnharmonic : KeyAccidental -> KeyAccidental
sharpenFlatEnharmonic ka =
    case ka of
        ( G, Flat ) ->
            ( F, Sharp )

        ( D, Flat ) ->
            ( C, Sharp )

        _ ->
            ka



{- we need to take note of any accidentals so far in the bar because these may influence
   later notes in that bar.  If the note uses an accidental, add it to the accidental set.
-}


addBarAccidental : PitchClass -> Maybe Accidental -> Accidentals -> Accidentals
addBarAccidental pc ma accs =
    case ma of
        Just acc ->
            Music.Accidentals.add pc acc accs

        _ ->
            accs



{- create a list of pairs which should match every possible
   note pitch  (pitch class and accidental) with its offset into
   its 12-note chromatic scale
-}


noteNumbers : List ( ( PitchClass, Accidental ), Int )
noteNumbers =
    [ ( ( C, Flat ), 11 )
    , ( ( C, Natural ), 0 )
    , ( ( C, Sharp ), 1 )
    , ( ( C, DoubleSharp ), 2 )
    , ( ( D, DoubleFlat ), 0 )
    , ( ( D, Flat ), 1 )
    , ( ( D, Natural ), 2 )
    , ( ( D, Sharp ), 3 )
    , ( ( D, DoubleSharp ), 4 )
    , ( ( E, DoubleFlat ), 2 )
    , ( ( E, Flat ), 3 )
    , ( ( E, Natural ), 4 )
    , ( ( E, Sharp ), 5 )
    , ( ( E, DoubleSharp ), 6 )
    , ( ( F, Flat ), 4 )
    , ( ( F, Natural ), 5 )
    , ( ( F, Sharp ), 6 )
    , ( ( F, DoubleSharp ), 7 )
    , ( ( G, DoubleFlat ), 5 )
    , ( ( G, Flat ), 6 )
    , ( ( G, Natural ), 7 )
    , ( ( G, Sharp ), 8 )
    , ( ( G, DoubleSharp ), 9 )
    , ( ( A, DoubleFlat ), 7 )
    , ( ( A, Flat ), 8 )
    , ( ( A, Natural ), 9 )
    , ( ( A, Sharp ), 10 )
    , ( ( A, DoubleSharp ), 11 )
    , ( ( B, DoubleFlat ), 9 )
    , ( ( B, Flat ), 10 )
    , ( ( B, Natural ), 11 )
    , ( ( B, Sharp ), 0 )
    , ( ( B, DoubleSharp ), 1 )
    ]



{- note pairs for the black and white notes of a piano,
   designating black notes with the Sharp accidental
-}


sharpNoteNumbers : List ( ( PitchClass, Accidental ), Int )
sharpNoteNumbers =
    let
        f nn =
            let
                ( ( pc, a ), i ) =
                    nn
            in
                ((a == Sharp) && (pc /= E && pc /= B))
                    || (a == Natural)
    in
        List.filter f noteNumbers



{- note pairs for the black and white notes of a piano,
   designating black notes with the Flat accidental
-}


flatNoteNumbers : List ( ( PitchClass, Accidental ), Int )
flatNoteNumbers =
    let
        f nn =
            let
                ( ( pc, a ), i ) =
                    nn
            in
                ((a == Flat) && (pc /= F && pc /= C))
                    || (a == Natural)
    in
        List.filter f noteNumbers



{- given a key signature and an integer (0 <= n < notesInChromaticScale)
   return the pitch of the note within that signature
-}


pitchFromInt : KeySignature -> Int -> KeyAccidental
pitchFromInt ks i =
    let
        dict =
            if (isCOrSharpKey ks) then
                sharpNotedNumbers
            else
                flatNotedNumbers
    in
        Dict.get i dict
            |> withDefault ( C, Natural )



{- the inverted lookup for sharp chromatic scales.  This dictionaary
   allows you to enter a number (0 <= n < notesInChromaticScale) and return
   a (pitchClass, Accidental) pair which is the note's pitch
-}


sharpNotedNumbers : Dict Int ( PitchClass, Accidental )
sharpNotedNumbers =
    let
        invert ( a, b ) =
            ( b, a )
    in
        List.map invert sharpNoteNumbers
            |> Dict.fromList



{- the inverted lookup for flat chromatic scales.  This dictionaary
   allows you to enter a number (0 <= n < notesInChromaticScale) and return
   a (pitchClass, Accidental) pair which is the note's pitch
-}


flatNotedNumbers : Dict Int ( PitchClass, Accidental )
flatNotedNumbers =
    let
        invert ( a, b ) =
            ( b, a )
    in
        List.map invert flatNoteNumbers
            |> Dict.fromList



{- make a note's pitch comparable by translating to a string
   so it can be used in dictionaries
-}


comparableNote : ( PitchClass, Accidental ) -> String
comparableNote n =
    let
        ( pc, acc ) =
            n

        accStr =
            case acc of
                Natural ->
                    ""

                Sharp ->
                    "#"

                Flat ->
                    "b"

                DoubleSharp ->
                    "##"

                DoubleFlat ->
                    "bb"
    in
        toString pc ++ accStr



{- noteNumbers with the notes converted to comparable strings for use in dictionaries -}


comparableNoteNumbers : List ( String, Int )
comparableNoteNumbers =
    let
        f notePair =
            ( comparableNote (fst notePair), (snd notePair) )
    in
        List.map f noteNumbers



{- a dictionary of comparable note -> note number -}


chromaticScaleDict : Dict String Int
chromaticScaleDict =
    Dict.fromList comparableNoteNumbers


lookupChromatic : Dict String Int -> String -> Int
lookupChromatic dict target =
    Dict.get target dict
        |> withDefault 0



{- look up the pitch and return a number in the range 0 <= n < notesInChromaticScale  (0 is C Natural) -}


pitchNumber : ( PitchClass, Accidental ) -> Int
pitchNumber pa =
    lookupChromatic chromaticScaleDict (comparableNote pa)



{- look up the note and return the number of its pitch in the range 0 <= n < notesInChromaticScale (0 is C Natural) -}


noteNumber : AbcNote -> Int
noteNumber n =
    let
        acc =
            n.accidental
                |> withDefault Natural
    in
        pitchNumber ( n.pitchClass, acc )



{- inspect the current note index and the amount it is to be incremented by.
   produce a new note index in the range (0 <= n < notesInChromaticScale)
   and associate with this a number (-1,0,1) which indicates an increment to the octave
-}


noteIndex : Int -> Int -> ( Int, Int )
noteIndex from increment =
    let
        -- _ = log "note index from" from
        -- _ = log "note index by" increment
        to =
            (from + increment)
    in
        if to < 0 then
            ( (notesInChromaticScale + to), -1 )
        else if (to >= notesInChromaticScale) then
            ( (to - notesInChromaticScale), 1 )
        else
            ( to, 0 )



{- work out the minimum transposition distance (target - source) or (source - target) measured in semitones -}


transpositionDistance : ( PitchClass, Maybe Accidental ) -> ( PitchClass, Maybe Accidental ) -> Int
transpositionDistance target src =
    let
        ( spc, smacc ) =
            src

        ( tpc, tmacc ) =
            target

        sacc =
            smacc |> withDefault Natural

        tacc =
            tmacc |> withDefault Natural

        distance =
            (pitchNumber ( tpc, tacc ) - pitchNumber ( spc, sacc ))
    in
        -- +5 is a shorter distance than -7 etc
        if (distance <= -7) then
            (notesInChromaticScale + distance) % notesInChromaticScale
        else
            distance



{- replace a Key header (if it exists) -}


replaceKeyHeader : ModifiedKeySignature -> TuneHeaders -> TuneHeaders
replaceKeyHeader newmks hs =
    let
        f h =
            case h of
                Key mks ->
                    False

                _ ->
                    True

        newhs =
            List.filter f hs
    in
        newhs ++ [ Key newmks ]
