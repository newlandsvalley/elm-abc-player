port module SoundFont.Ports exposing (..)

import SoundFont.Types exposing (..)


-- outgoing ports (for commands to javascript)


{-| request the AudioContext
-}
port initialiseAudioContext : () -> Cmd msg


{-| ask if the browser supports the OGG format
-}
port requestIsOggEnabled : () -> Cmd msg


{-| request that the default piano fonts are loaded from the local resource
    under the given directory.  By local we mean that the soundfonts are
    housed on the same server as the requesting code
-}
port requestLoadPianoFonts : String -> Cmd msg


{-| request that the font for the named instrument is loaded from the
    remote gleitz github server (i.e. where they are maintained).
    This will take longer to load than the local resource above.
-}
port requestLoadRemoteFonts : String -> Cmd msg


{-| request that the note is played through the soundfont via web-audio
-}
port requestPlayNote : MidiNote -> Cmd msg


{-| request that the sequence of notes is played through the soundfont via web-audio
-}
port requestPlayNoteSequence : MidiNotes -> Cmd msg



-- incoming ports (for subscriptions from javascript)


{-| get the audio context.
 Probably not much use because it is incomplete and cannot be passed back to javascript
-}
port getAudioContext : (AudioContext -> msg) -> Sub msg


{-| does the browser support the Ogg-Vorbis standard?
-}
port oggEnabled : (Bool -> msg) -> Sub msg


{-| Have the soundfonts been loaded OK?
-}
port fontsLoaded : (Bool -> msg) -> Sub msg


{-| Have we played the individual note?
-}
port playedNote : (Bool -> msg) -> Sub msg


{-| Have we started to play the note sequence?
-}
port playSequenceStarted : (Bool -> msg) -> Sub msg
