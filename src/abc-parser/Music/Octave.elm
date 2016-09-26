module Music.Octave
    exposing
        ( up
        , down
        )

{-| Move a tune up or down an octave


# Definition

# Functions
@docs up
    , down

-}

import Abc.ParseTree exposing (..)


-- Exposed API


{-| Move the tune up an octave
-}
up : AbcTune -> AbcTune
up t =
    moveTune 1 t


{-| Move the tune down octave
-}
down : AbcTune -> AbcTune
down t =
    moveTune -1 t



-- Implementation


moveTune : Int -> AbcTune -> AbcTune
moveTune i t =
    let
        ( headers, body ) =
            t
    in
        ( headers, (moveTuneBody i body) )


moveTuneBody : Int -> TuneBody -> TuneBody
moveTuneBody i =
    List.map (moveBodyPart i)


moveBodyPart : Int -> BodyPart -> BodyPart
moveBodyPart i bp =
    case bp of
        Score ms ->
            Score (moveMusicList i ms)

        _ ->
            bp


moveOctave : Int -> Music -> Music
moveOctave i m =
    case m of
        Note n ->
            Note (moveNoteBy i n)

        BrokenRhythmPair n1 b n2 ->
            BrokenRhythmPair (moveNoteBy i n1) b (moveNoteBy i n2)

        Tuplet ts ns ->
            Tuplet ts (moveNoteList i ns)

        GraceNote b ns ->
            GraceNote b (moveNoteList i ns)

        Chord c ->
            Chord (moveChord i c)

        _ ->
            m


moveNoteBy : Int -> AbcNote -> AbcNote
moveNoteBy i note =
    { note | octave = note.octave + i }


moveMusicList : Int -> List Music -> List Music
moveMusicList i =
    List.map (moveOctave i)


moveNoteList : Int -> List AbcNote -> List AbcNote
moveNoteList i =
    List.map (moveNoteBy i)


moveChord : Int -> AbcChord -> AbcChord
moveChord i c =
    { c | notes = moveNoteList i c.notes }
