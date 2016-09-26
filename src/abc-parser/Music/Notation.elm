module Music.Notation
    exposing
        ( MidiPitch
        , MidiTick
        , AbcTempo
        , NoteTime
        , DiatonicScale
        , HeaderMap
        , notesInChromaticScale
        , standardMidiTick
        , keySet
        , modifiedKeySet
        , getKeySet
        , inKeySet
        , getHeaderMap
        , getKeySig
        , getTitle
        , diatonicScale
        , inScale
        , isCOrSharpKey
        , accidentalImplicitInKey
        , dotFactor
        , noteTicks
        , chordalNoteTicks
        , toMidiPitch
        , midiTempo
        , noteDuration
        , chordalNoteDuration
        , transposeKeySignatureBy
        , normaliseModalKey
        )

{-| Helper functions for making more musical sense of the parse tree

# Definition

# Data Types
@docs MidiPitch, MidiTick, AbcTempo, NoteTime, DiatonicScale, HeaderMap

# Functions
@docs notesInChromaticScale
    , standardMidiTick
    , keySet
    , modifiedKeySet
    , getKeySet
    , inKeySet
    , getHeaderMap
    , getKeySig
    , getTitle
    , diatonicScale
    , inScale
    , isCOrSharpKey
    , accidentalImplicitInKey
    , dotFactor
    , noteTicks
    , chordalNoteTicks
    , toMidiPitch
    , midiTempo
    , noteDuration
    , chordalNoteDuration
    , transposeKeySignatureBy
    , normaliseModalKey

-}

import List.Extra exposing (getAt, splitAt, elemIndex, tails)
import List exposing (member, isEmpty)
import Maybe exposing (withDefault, oneOf)
import Maybe.Extra exposing (join, isJust)
import String exposing (contains, endsWith, fromChar)
import Dict exposing (Dict, fromList, get)
import Abc.ParseTree exposing (..)
import Ratio exposing (..)
import Music.Accidentals exposing (..)
import Debug exposing (..)


{-| a diatonic scale presented as a list of notes in the scale
-}
type alias DiatonicScale =
    List KeyAccidental



{- a chromatic (11-note) scale -}


type alias ChromaticScale =
    List KeyAccidental


type alias Intervals =
    List Int


{-| the pitch of a note expressed as a MIDI interval
-}
type alias MidiPitch =
    Int


{-| a MIDI tick - used to give a note duration
-}
type alias MidiTick =
    Int


{-| the time taken when a note is played before the next note
-}
type alias NoteTime =
    Float


{-| ABC header information defining tempo
-}
type alias AbcTempo =
    { tempoNoteLength : Rational
    , bpm : Int
    , unitNoteLength : Rational
    }


{-| reconstitute the map of Header characters to the headers themselves
    taking the first instance of any header in each case
-}
type alias HeaderMap =
    Dict Char Header



-- EXPORTED FUNCTIONS


{-| a standard MIDI tick - we'll use 1/4 note = 480 ticks
-}
standardMidiTick : MidiTick
standardMidiTick =
    480


{-| there are 12 notes in a chromatic scale
-}
notesInChromaticScale : Int
notesInChromaticScale =
    12


{-| return the set of keys (pitch classes with accidental) that comprise the key signature
-}
keySet : KeySignature -> KeySet
keySet ks =
    diatonicScale ks
        |> List.filter accidentalKey


{-| return the set of keys (pitch classes with accidental) that comprise a modified key signature
-}
modifiedKeySet : ModifiedKeySignature -> KeySet
modifiedKeySet ksm =
    let
        ( ksig, mods ) =
            ksm

        ks =
            keySet ksig
    in
        if (isEmpty mods) then
            ks
        else
            List.foldr modifyKeySet ks mods


{-| get set of key accidentals from the (possibly modified) key (if there is one in the tune)
-}
getKeySet : AbcTune -> KeySet
getKeySet t =
    let
        mksig =
            getKeySig t
    in
        case mksig of
            Just ksig ->
                modifiedKeySet ksig

            Nothing ->
                []


{-| return True if the KeyAccidental is in the KeySet
-}
inKeySet : KeyAccidental -> KeySet -> Bool
inKeySet ka ks =
    List.member ka ks


{-| return a map of header code to Header for the first instance of each Header
-}
getHeaderMap : AbcTune -> HeaderMap
getHeaderMap t =
    let
        f : Header -> ( Char, Header )
        f h =
            case h of
                Area _ ->
                    ( 'A', h )

                Book _ ->
                    ( 'B', h )

                Composer _ ->
                    ( 'C', h )

                Discography _ ->
                    ( 'D', h )

                FileUrl _ ->
                    ( 'F', h )

                Group _ ->
                    ( 'G', h )

                History _ ->
                    ( 'H', h )

                Instruction _ ->
                    ( 'I', h )

                Key _ ->
                    ( 'K', h )

                UnitNoteLength _ ->
                    ( 'L', h )

                Meter _ ->
                    ( 'M', h )

                Macro _ ->
                    ( 'm', h )

                Notes _ ->
                    ( 'N', h )

                Origin _ ->
                    ( 'O', h )

                Parts _ ->
                    ( 'P', h )

                Tempo _ ->
                    ( 'Q', h )

                Rhythm _ ->
                    ( 'R', h )

                Remark _ ->
                    ( 'r', h )

                Source _ ->
                    ( 'S', h )

                SymbolLine _ ->
                    ( 's', h )

                Title _ ->
                    ( 'T', h )

                UserDefined _ ->
                    ( 'U', h )

                Voice _ ->
                    ( 'V', h )

                WordsAfter _ ->
                    ( 'W', h )

                WordsAligned _ ->
                    ( 'w', h )

                ReferenceNumber _ ->
                    ( 'X', h )

                Transcription _ ->
                    ( 'Z', h )

                FieldContinuation _ ->
                    ( '+', h )

                Comment _ ->
                    ( '-', h )

                UnsupportedHeader ->
                    ( 'u', h )

        annotatedHeaders =
            fst t
                |> List.reverse
                |> List.map f
    in
        Dict.fromList annotatedHeaders


{-| get the key signature (if any) from the tune
-}
getKeySig : AbcTune -> Maybe ModifiedKeySignature
getKeySig t =
    let
        headerMap =
            getHeaderMap t
    in
        case Dict.get 'K' headerMap of
            Just kh ->
                case kh of
                    Key mks ->
                        Just mks

                    _ ->
                        Nothing

            _ ->
                Nothing



{- old implementation
   getKeySig t =
       let
           headers =
               fst t

           f h =
               case h of
                   Key mks ->
                       Just mks

                   _ ->
                       Nothing

           ksigs =
               List.map f headers
                   |> List.filter isJust
       in
           List.head ksigs
               |> join
-}


{-| get the first Title (if any) from the tune
-}
getTitle : AbcTune -> Maybe String
getTitle t =
    let
        headerMap =
            getHeaderMap t
    in
        case Dict.get 'T' headerMap of
            Just th ->
                case th of
                    Title title ->
                        Just title

                    _ ->
                        Nothing

            _ ->
                Nothing



{- old implementation
   getTitle t =
       let
           headers =
               fst t

           f h =
               case h of
                   Title title ->
                       Just title

                   _ ->
                       Nothing

           titles =
               List.map f headers
                   |> List.filter isJust
       in
           List.head titles
               |> join
-}


{-| return the set of keys (pitch classes and accidental) that comprise a diatonic scale
-}
diatonicScale : KeySignature -> DiatonicScale
diatonicScale ks =
    let
        accidental =
            case ks.accidental of
                Just a ->
                    a

                Nothing ->
                    Natural

        target =
            ( ks.pitchClass, accidental )
    in
        case ks.mode of
            Major ->
                majorScale target

            Ionian ->
                majorScale target

            m ->
                modalScale target ks.mode


{-| return True if the KeyAccidental is in the (diatonic) scale
-}
inScale : KeyAccidental -> DiatonicScale -> Bool
inScale ka s =
    List.member ka s


{-| return True if the key signature is a sharp key or a simple C Major key
-}
isCOrSharpKey : KeySignature -> Bool
isCOrSharpKey ksig =
    let
        kset =
            keySet ksig

        -- if we make the default (for an empty list) as a samplemsharp then we return true for the scale of C Major
        ( samplePC, sampleAcc ) =
            List.head kset
                |> withDefault ( C, Sharp )
    in
        sampleAcc == Sharp


{-| return an accidental if it is implicitly there in the (modified) key signature
    attached to the pitch class of the note
-}
accidentalImplicitInKey : PitchClass -> ModifiedKeySignature -> Maybe Accidental
accidentalImplicitInKey pc mks =
    let
        keyset =
            modifiedKeySet mks

        accidentals =
            Music.Accidentals.fromKeySet keyset
    in
        lookup pc accidentals



-- accidentalInKeySet n (modifiedKeySet mks)
{- modify a key set with a new accidental -}


modifyKeySet : KeyAccidental -> KeySet -> KeySet
modifyKeySet target ks =
    let
        ( pc, accidental ) =
            target

        -- filter out the given pitch class of the incoming key accidental
        f key =
            (fst key /= pc)

        newks =
            List.filter f ks
    in
        -- if it's a natural, just remove any old sharp or flat key from the incoming key accidental
        if (accidental == Natural) then
            ks
        else
            -- otherwise, add the incoming key accidental
            target :: ks


{-| the amount by which you increase the duration of a (multiply) dotted note
   i.e. duration of a note dotted by x is multiplied by:
      1 + dotFactor x
   and of one symmetrically reduced is
      1 - dotfactor x
-}
dotFactor : Int -> Rational
dotFactor i =
    case i of
        1 ->
            1 `over` 2

        2 ->
            3 `over` 4

        3 ->
            7 `over` 8

        _ ->
            0 `over` 1


{-| find a real world note duration by translating an ABC note duration using a tempo and unit note length
-}
noteDuration : AbcTempo -> Rational -> NoteTime
noteDuration t n =
    (60.0 * (Ratio.toFloat t.unitNoteLength) * (Ratio.toFloat n))
        / ((Ratio.toFloat t.tempoNoteLength) * (Basics.toFloat t.bpm))


{-| find a real world duration of a note in a chord by translating an ABC note duration together with a chord duration using a tempo and unit note length
-}
chordalNoteDuration : AbcTempo -> Rational -> Rational -> NoteTime
chordalNoteDuration t note chord =
    (60.0 * (Ratio.toFloat t.unitNoteLength) * (Ratio.toFloat note) * (Ratio.toFloat chord))
        / ((Ratio.toFloat t.tempoNoteLength) * (Basics.toFloat t.bpm))



-- MIDI support


{-| Calculate a MIDI note duration from the note length

    Assume a standard unit note length of 1/4 and a standard number of ticks per unit (1/4) note of 480
-}
noteTicks : Rational -> MidiTick
noteTicks n =
    (standardMidiTick * (numerator n)) // (denominator n)



-- // is used for integer division in elm


{-| find a MIDI duration of a note within a chord in standard ticks (as above)
    (1/4 note == 480 ticks)
-}
chordalNoteTicks : Rational -> Rational -> MidiTick
chordalNoteTicks note chord =
    let
        nTicks =
            noteTicks note
    in
        (nTicks * (numerator chord)) // (denominator chord)



-- // is used for integer division in elm


{-| convert an ABC note pitch to a MIDI pitch
   AbcNote - the note in question
   ModifiedKeySignature - the key signature (possibly modified by extra accidentals)
   Accidentals - any notes in this bar which have previously been set explicitly to an accidental which are thus inherited by this note
   MidiPitch - the resulting pitch of the MIDI note

-}
toMidiPitch : AbcNote -> ModifiedKeySignature -> Accidentals -> MidiPitch
toMidiPitch n mks barAccidentals =
    (n.octave * notesInChromaticScale) + midiPitchOffset n mks barAccidentals


{-| the MIDI tempo is measured in microseconds per beat

    algorithm is:

    t.bpm beats occupy 1 minute or 60 * 10^16 μsec
    1 bpm beat occupies 60 * 10^16/t.bpm μsec

    but we use a standard beat of 1 unit when writing a note, whereas the bpm measures a tempo note length of
    t.unitNoteLength/t.tempoNoteLength
    i.e.
    1 whole note beat occupies 60 * 10^16/t.bpm * t.unl/t.tnl μsec

-}
midiTempo : AbcTempo -> Int
midiTempo t =
    let
        relativeNoteLength =
            divide t.unitNoteLength t.tempoNoteLength
    in
        round ((60.0 * 1000000 * Basics.toFloat (numerator relativeNoteLength)) / (Basics.toFloat t.bpm * Basics.toFloat (denominator relativeNoteLength)))


{-| transpose a key signature by a given distance
-}
transposeKeySignatureBy : Int -> ModifiedKeySignature -> ModifiedKeySignature
transposeKeySignatureBy i mks =
    let
        ( ks, keyaccs ) =
            mks

        -- turn the key sig to a string pattern and look up its index
        pattern =
            (toString ks.pitchClass) ++ (accidentalPattern ks.accidental)

        index =
            withDefault 0 (Dict.get pattern chromaticScaleDict)

        newIndex =
            (notesInChromaticScale + index + i) % notesInChromaticScale

        -- keep hold of the sharp / flat nature of the original scale
        scale =
            if (isCOrSharpKey ks) then
                sharpScale
            else
                flatScale

        -- now look up the transposed key at the new index
        ( pc, ma ) =
            getAt newIndex scale
                |> withDefault ( C, Natural )

        -- modify the key accidentals likewise
        accs =
            List.map (transposeKeyAccidentalBy i) keyaccs

        -- build the most likely enharmonic equivalent - don't use bizarre keys
        newks =
            equivalentEnharmonicKeySig pc ma ks.mode
    in
        ( newks, accs )



-- implementation
{- works from C major up to B major but not beyond
    (F# major requires F->E#, C# major also requires C->B#)
   "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"
-}


sharpScale : ChromaticScale
sharpScale =
    [ ( C, Natural )
    , ( C, Sharp )
    , ( D, Natural )
    , ( D, Sharp )
    , ( E, Natural )
    , ( F, Natural )
    , ( F, Sharp )
    , ( G, Natural )
    , ( G, Sharp )
    , ( A, Natural )
    , ( A, Sharp )
    , ( B, Natural )
    ]



-- "B#", "C#", "D", "D#", "E", "E#", "F#", "G", "G#", "A", "A#", "B"


extremeSharpScale : ChromaticScale
extremeSharpScale =
    let
        f pc =
            case pc of
                ( F, Natural ) ->
                    ( E, Sharp )

                ( C, Natural ) ->
                    ( B, Sharp )

                _ ->
                    pc
    in
        List.map f sharpScale



{- works from C major down to Db major but not beyond
   (Gb major requires B->Cb, Cb major also requires E->Fb)
   "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"
-}


flatScale : ChromaticScale
flatScale =
    [ ( C, Natural )
    , ( D, Flat )
    , ( D, Natural )
    , ( E, Flat )
    , ( E, Natural )
    , ( F, Natural )
    , ( G, Flat )
    , ( G, Natural )
    , ( A, Flat )
    , ( A, Natural )
    , ( B, Flat )
    , ( B, Natural )
    ]



-- "C", "Db", "D", "Eb", "Fb", "F", "Gb", "G", "Ab", "A", "Bb", "Cb"


extremeFlatScale : ChromaticScale
extremeFlatScale =
    let
        f pc =
            case pc of
                ( E, Natural ) ->
                    ( F, Flat )

                ( B, Natural ) ->
                    ( C, Flat )

                _ ->
                    pc
    in
        List.map f flatScale



{- enharmonic equivalence of key classes - don't use bizarre sharp keys when we have reasonable flat ones -}


equivalentEnharmonic : KeyAccidental -> KeyAccidental
equivalentEnharmonic k =
    case k of
        ( A, Sharp ) ->
            ( B, Flat )

        ( C, Sharp ) ->
            ( D, Flat )

        ( D, Sharp ) ->
            ( E, Flat )

        ( G, Sharp ) ->
            ( A, Flat )

        _ ->
            k


sharpScaleEquivalent : KeyAccidental -> KeyAccidental
sharpScaleEquivalent ka =
    case (snd ka) of
        Flat ->
            let
                index =
                    elemIndex ka flatScale
                        |> withDefault 0
            in
                lookUpScale sharpScale index

        _ ->
            ka



{- enharmonic equivalence of full key signatures - don't use bizarre minor flat keys when we have reasonable sharp ones
   and vice versa.  Check both Major and Monor key signatures.
-}


equivalentEnharmonicKeySig : PitchClass -> Accidental -> Mode -> KeySignature
equivalentEnharmonicKeySig pc a m =
    case ( pc, a, m ) of
        -- major key signatures
        ( A, Sharp, Major ) ->
            { pitchClass = B, accidental = Just Flat, mode = Major }

        ( D, Sharp, Major ) ->
            { pitchClass = E, accidental = Just Flat, mode = Major }

        ( G, Sharp, Major ) ->
            { pitchClass = A, accidental = Just Flat, mode = Major }

        -- minor key signatures
        ( G, Flat, Minor ) ->
            { pitchClass = F, accidental = Just Sharp, mode = Minor }

        ( D, Flat, Minor ) ->
            { pitchClass = C, accidental = Just Sharp, mode = Minor }

        ( A, Flat, Minor ) ->
            { pitchClass = G, accidental = Just Sharp, mode = Minor }

        _ ->
            { pitchClass = pc, accidental = Just a, mode = m }


majorIntervals : Intervals
majorIntervals =
    [ 2, 2, 1, 2, 2, 2, 1 ]



{- lookup for providing offsets from C in a chromatic scale
   we have to translate a KeyAccidental to a string because otherwise
   it can't be used as a Dict key.  This is a problem in Elm -
   user-defined types should be attributable to a 'pseudo'
   type class of comparable
-}


chromaticScaleDict : Dict String Int
chromaticScaleDict =
    Dict.fromList
        [ ( "C", 0 )
        , ( "C#", 1 )
        , ( "Db", 1 )
        , ( "D", 2 )
        , ( "D#", 3 )
        , ( "Eb", 3 )
        , ( "E", 4 )
        , ( "F", 5 )
        , ( "F#", 6 )
        , ( "Gb", 6 )
        , ( "G", 7 )
        , ( "G#", 8 )
        , ( "Ab", 8 )
        , ( "A", 9 )
        , ( "A#", 10 )
        , ( "Bb", 10 )
        , ( "B", 11 )
        ]



-- rotate the chromatic scale, starting from the supplied target character


rotateFrom : KeyAccidental -> ChromaticScale -> ChromaticScale
rotateFrom target scale =
    let
        index =
            elemIndex target scale
                |> withDefault 0

        listPair =
            splitAt index scale
    in
        List.append (snd listPair) (fst listPair)



-- rotate an interval pattern to the left by the given amount


rotateLeftBy : Int -> Intervals -> Intervals
rotateLeftBy index ls =
    let
        listPair =
            splitAt index ls
    in
        List.append (snd listPair) (fst listPair)



{- convert e.g. [2,2,1,2,2,2,1] into [0,2,4,5,7,9,11]
   so instead of tone/semitone intervals we have offsets into a chromatic scale
-}


partialSum : List Int -> List Int
partialSum l =
    List.map List.sum (tails (List.reverse l))
        |> List.reverse
        |> List.take (List.length l)



{- find the pitch class at a given position in the scale
   modulate the index to be in the range 0 <= index < notesInChromaticScale
   where a negative value rotates left from the maximum value in the scale
-}


lookUpScale : ChromaticScale -> Int -> KeyAccidental
lookUpScale s i =
    let
        modi =
            i % notesInChromaticScale

        index =
            if (modi < 0) then
                notesInChromaticScale - modi
            else
                modi
    in
        getAt index s
            |> withDefault ( C, Natural )



-- provide the Major scale for the pitch class


majorScale : KeyAccidental -> DiatonicScale
majorScale target =
    let
        chromaticScale =
            if (target == ( G, Flat ) || target == ( C, Flat )) then
                extremeFlatScale
            else if (isFlatMajorKey target) then
                flatScale
            else if (target == ( F, Sharp ) || target == ( C, Sharp )) then
                extremeSharpScale
            else
                sharpScale

        f =
            lookUpScale (rotateFrom target chromaticScale)
    in
        List.map f (partialSum majorIntervals)



-- provide a Modal scale for the pitch class


modalScale : KeyAccidental -> Mode -> DiatonicScale
modalScale target mode =
    let
        distance =
            case mode of
                Minor ->
                    3

                Major ->
                    0

                _ ->
                    modalDistance mode

        index =
            elemIndex target sharpScale
                |> withDefault 0

        majorKeyIndex =
            (index + distance) % notesInChromaticScale

        majorKey =
            lookUpScale sharpScale majorKeyIndex
    in
        majorScale (equivalentEnharmonic majorKey)


{-| normalise a modal key signature to that of the equivalent major key

  Maybe, once this is completed and tested, implement modalScale in terms of this

-}
normaliseModalKey : KeySignature -> KeySignature
normaliseModalKey ks =
    let
        distance =
            modalDistance ks.mode

        sourceAccidental =
            maccToAcc ks.accidental

        scale =
            case ks.accidental of
                Just Sharp ->
                    sharpScale

                Just Flat ->
                    flatScale

                _ ->
                    case ks.pitchClass of
                        F ->
                            flatScale

                        _ ->
                            sharpScale

        keyAccidental =
            ( ks.pitchClass, sourceAccidental )

        index =
            elemIndex keyAccidental scale
                |> withDefault 0

        majorKeyIndex =
            (index + distance) % notesInChromaticScale

        majorKeyAcc =
            lookUpScale scale majorKeyIndex

        targetAccidental =
            accToMacc (snd majorKeyAcc)
    in
        if (0 == distance) then
            ks
        else
            { pitchClass = fst majorKeyAcc
            , accidental = targetAccidental
            , mode = Major
            }



{- convert a Maybe Accidental (used in key signatures)
   to an explict accidental (used in scales) where the
   explict form uses Natural
-}


maccToAcc : Maybe Accidental -> Accidental
maccToAcc macc =
    case macc of
        Just Sharp ->
            Sharp

        Just Flat ->
            Flat

        _ ->
            Natural



{- convert an explict accidental (used in scales)
   to a Maybe Accidental (used in key signatures) where the
   explict form uses Natural
-}


accToMacc : Accidental -> Maybe Accidental
accToMacc acc =
    case acc of
        Sharp ->
            Just Sharp

        Flat ->
            Just Flat

        _ ->
            Nothing



{- calculate the distance of the mode in semitones from Major (Ionian) -}


modalDistance : Mode -> Int
modalDistance mode =
    case mode of
        -- the distance to move right round the major scale
        Dorian ->
            10

        Phrygian ->
            8

        Lydian ->
            7

        Mixolydian ->
            5

        Aeolian ->
            3

        Minor ->
            3

        Locrian ->
            1

        _ ->
            0



{- return true if the key contains a sharp or flat accidental -}


accidentalKey : KeyAccidental -> Bool
accidentalKey k =
    let
        ( pc, acc ) =
            k
    in
        acc /= Natural



{- return true if the key represents a flat major key -}


isFlatMajorKey : KeyAccidental -> Bool
isFlatMajorKey target =
    let
        ( pc, accidental ) =
            target
    in
        case accidental of
            Natural ->
                (pc == F)

            a ->
                (a == Flat)



{- convert an AbcNote (pich class and accidental) to a pitch offset in a chromatic scale -}


midiPitchOffset : AbcNote -> ModifiedKeySignature -> Accidentals -> Int
midiPitchOffset n mks barAccidentals =
    let
        inBarAccidental =
            Music.Accidentals.lookup n.pitchClass barAccidentals

        inKeyAccidental =
            accidentalImplicitInKey n.pitchClass mks

        -- look first for an explicit note accidental, then for an explicit for the same note that occurred earlier in the bar and
        -- finally look for an implicit accidental attached to this key signature
        maybeAccidental =
            oneOf [ n.accidental, inBarAccidental, inKeyAccidental ]

        accidental =
            accidentalPattern maybeAccidental

        pattern =
            (toString n.pitchClass) ++ accidental
    in
        withDefault 0 (Dict.get pattern chromaticScaleDict)



{- turn an optional accidental into a string pattern for use in lookups -}


accidentalPattern : Maybe Accidental -> String
accidentalPattern ma =
    let
        f a =
            case a of
                Sharp ->
                    "#"

                Flat ->
                    "b"

                _ ->
                    ""
    in
        withDefault "" (Maybe.map f ma)



{- transpose a key accidental  (a key signature modifier)
   not finished
-}


transposeKeyAccidentalBy : Int -> KeyAccidental -> KeyAccidental
transposeKeyAccidentalBy i ka =
    let
        ( sourcepc, sourceacc ) =
            ka

        pattern =
            toString sourcepc

        index =
            withDefault 0 (Dict.get pattern chromaticScaleDict)

        ( modifier, scale ) =
            case sourceacc of
                Sharp ->
                    ( 1, sharpScale )

                Flat ->
                    ( -1, flatScale )

                DoubleSharp ->
                    ( 2, sharpScale )

                DoubleFlat ->
                    ( -2, flatScale )

                Natural ->
                    ( 0, sharpScale )
    in
        getAt (index + modifier + i) scale
            |> withDefault ( C, Natural )
