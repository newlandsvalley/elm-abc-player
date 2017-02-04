module SoundFont.Msg exposing (..)

import SoundFont.Types exposing (..)


type Msg
    = InitialiseAudioContext
    | ResponseAudioContext AudioContext
    | RequestOggEnabled
    | ResponseOggEnabled Bool
    | RequestLoadPianoFonts String
    | RequestLoadRemoteFonts String
    | ResponseFontsLoaded Bool
    | RequestPlayNote MidiNote
    | ResponsePlayedNote Bool
    | RequestPlayNoteSequence MidiNotes
    | ResponsePlaySequenceStarted Bool
    | NoOp
