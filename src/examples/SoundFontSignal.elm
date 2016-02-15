import Graphics.Element exposing (..)
import Maybe exposing (..)
import SoundFont exposing (..)

perhaps : Maybe SoundSample -> String
perhaps m = case m of
    Just ss -> ss.name
    Nothing -> "Nothing"

-- try to load a snare sample
pianoSample : Signal (Maybe SoundSample)
pianoSample = loadSoundFont  "acoustic_grand_piano"

main : Signal Element
main =
  Signal.map (show << perhaps) pianoSample
