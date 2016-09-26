module Abc.Canonical
    exposing
        ( fromTune
        , fromResult
        , abcNote
        , abcChord
        , tuplet
        )

{-| Module for converting an ABC Tune parse tree to a canonical ABC string,

   # Definition

   # Functions
   @docs fromTune, fromResult, abcNote, abcChord, tuplet

-}

import Abc.ParseTree exposing (..)
import Ratio exposing (Rational, numerator, denominator)
import Maybe exposing (withDefault)
import String exposing (fromChar, fromList, repeat, trimRight, toLower)


enquote : String -> String
enquote s =
    "\"" ++ s ++ "\""


mode : Mode -> String
mode m =
    case m of
        Major ->
            "major"

        Minor ->
            "minor"

        Ionian ->
            "ionian"

        Dorian ->
            "dorian"

        Phrygian ->
            "phrygian"

        Lydian ->
            "lydian"

        Mixolydian ->
            "mixolydian"

        Aeolian ->
            "aeolian"

        Locrian ->
            "locrian"


bar : Bar -> String
bar b =
    let
        it =
            withDefault "" (Maybe.map toString b.iteration)

        lines =
            case b.thickness of
                ThickThin ->
                    "[|"

                ThinThick ->
                    "|]"

                ThinThin ->
                    "||"

                _ ->
                    "|"
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
accidental a =
    case a of
        Sharp ->
            "^"

        Flat ->
            "_"

        DoubleSharp ->
            "^^"

        DoubleFlat ->
            "__"

        Natural ->
            "="


headerAccidental : Accidental -> String
headerAccidental a =
    case a of
        Sharp ->
            "#"

        Flat ->
            "b"

        _ ->
            ""

{- pretty print a tuplet
-}
tuplet : TupletSignature -> String
tuplet t =
    let
        ( p, q, r ) =
            t
    in
        case t of
            ( 2, 3, 2 ) ->
                "(2"

            ( 3, 2, 3 ) ->
                "(3"

            ( 4, 3, 4 ) ->
                "(4"

            _ ->
                "("
                    ++ (toString p)
                    ++ ":"
                    ++ (toString q)
                    ++ ":"
                    ++ (toString r)


tempo : TempoSignature -> String
tempo t =
    let
        text =
            withDefault "" (Maybe.map (\s -> " " ++ (enquote s)) t.marking)

        eq =
            if (List.length t.noteLengths == 0) then
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
        f r acc =
            (rational r) ++ " " ++ acc
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
                ( n, d ) =
                    m
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
        acc =
            Maybe.map headerAccidental k.accidental
                |> withDefault ""
    in
        toString k.pitchClass ++ acc ++ toString k.mode


keyAccidental : KeyAccidental -> String
keyAccidental ka =
    let
        ( pc, acc ) =
            ka
    in
        accidental acc ++ toLower (toString pc)


keyAccidentals : List KeyAccidental -> String
keyAccidentals =
    String.concat
        << List.map (\a -> " " ++ keyAccidental a)


octave : Int -> String
octave i =
    let
        octaveAboveMiddleC =
            middlecOctave + 1
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


{-| pretty-print a note
-}
abcNote : AbcNote -> String
abcNote a =
    let
        acc =
            Maybe.map accidental a.accidental
                |> withDefault ""

        tie =
            case a.tied of
                True ->
                    "-"

                _ ->
                    ""
    in
        acc
            ++ pitch a.octave a.pitchClass
            ++ octave a.octave
            ++ duration a.duration
            ++ tie

{-| pretty-print a chord
-}
abcChord : AbcChord -> String
abcChord a =
    "["
        ++ (notes a.notes)
        ++ "]"
        ++ duration a.duration


notes : List AbcNote -> String
notes ns =
    let
        f a acc =
            (abcNote a) ++ acc
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


musics : List Music -> String
musics ms =
    let
        f m acc =
            (music m) ++ acc
    in
        List.foldr f "" ms


broken : Broken -> String
broken b =
    case b of
        LeftArrow i ->
            String.repeat i "<"

        RightArrow i ->
            String.repeat i ">"


music : Music -> String
music m =
    case m of
        Barline b ->
            bar b

        Note a ->
            abcNote a

        BrokenRhythmPair a1 b a2 ->
            abcNote a1 ++ (broken b) ++ abcNote a2

        Rest r ->
            rest r

        Tuplet tup ns ->
            tuplet tup ++ notes ns

        Decoration s ->
            decorate s

        GraceNote isAcciaccatura ns ->
            "{" ++ notes ns ++ "}"

        Slur c ->
            String.fromChar c

        Annotation placement s ->
            toString placement ++ ":" ++ s

        ChordSymbol s ->
            enquote s

        Chord a ->
            abcChord a

        Inline h ->
            "[" ++ header h ++ "]"

        Spacer i ->
            " "

        Ignore ->
            ""

        Continuation ->
            "\\"



-- _ -> ""


header : Header -> String
header h =
    case h of
        Area s ->
            "A: " ++ s

        Book s ->
            "B: " ++ s

        Composer s ->
            "C: " ++ s

        Discography s ->
            "D: " ++ s

        FileUrl s ->
            "F: " ++ s

        Group s ->
            "G: " ++ s

        History s ->
            "H: " ++ s

        Instruction s ->
            "I: " ++ s

        Key ( k, kacc ) ->
            "K: " ++ (key k) ++ (keyAccidentals kacc)

        UnitNoteLength d ->
            "L: " ++ (duration d)

        Meter m ->
            "M: " ++ (meter m)

        Macro s ->
            "m: " ++ s

        Notes s ->
            "N: " ++ s

        Origin s ->
            "O: " ++ s

        Parts s ->
            "P: " ++ s

        Rhythm s ->
            "R: " ++ s

        Remark s ->
            "r: " ++ s

        Source s ->
            "S: " ++ s

        Title s ->
            "T: " ++ s

        Tempo t ->
            "Q: " ++ (tempo t)

        UserDefined s ->
            "U: " ++ s

        Voice s ->
            "V: " ++ s

        WordsAfter s ->
            "W: " ++ s

        WordsAligned s ->
            "w: " ++ s

        ReferenceNumber i ->
            "X: " ++ (toString i)

        Transcription s ->
            "Z: " ++ s

        FieldContinuation s ->
            "+: " ++ s

        Comment s ->
            "%" ++ s

        _ ->
            ""


tuneHeaders : List Header -> String
tuneHeaders hs =
    let
        f h acc =
            (header h) ++ "\x0D\n" ++ acc
    in
        List.foldr f "" hs


bodyPart : BodyPart -> String
bodyPart bp =
    case bp of
        Score ml ->
            musics ml

        BodyInfo h ->
            header h


continuation : Bool -> String
continuation c =
    if c then
        "\\"
    else
        ""


tuneBody : TuneBody -> String
tuneBody b =
    let
        f bp acc =
            (bodyPart bp) ++ "\x0D\n" ++ acc
    in
        List.foldr f "" b



-- Exported Functions


{-| translate an ABC Tune parse tree to a canonical ABC String
-}
fromTune : AbcTune -> String
fromTune t =
    tuneHeaders (fst t) ++ tuneBody (snd t)


{-| translate a parse Result containing an ABC Tune parse tree to a Result containing a canonical ABC String
-}
fromResult : Result String AbcTune -> Result String String
fromResult r =
    Result.map fromTune r
