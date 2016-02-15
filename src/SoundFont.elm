module SoundFont
    (  SoundSample
      ,SoundBite
      ,loadSoundFont
      ,getCurrentTime
      ,maybePlay
    ) where

{-|  Library for working with the WebAudio API using SoundFonts,

# Definition

# Data Types
@docs SoundSample, SoundBite

# Functions
@docs loadSoundFont, getCurrentTime, maybePlay

-}

import Native.SoundFont
import Task exposing (Task, andThen, mapError, succeed)
import Maybe exposing (Maybe)
import Effects exposing (Never)

{-| AudioBuffers -}
type AudioBuffer = AudioBuffer

{-| Sound Samples -}
type alias SoundSample =
  { name   : String
   ,buffer : AudioBuffer
  }

{-| Sound Bites -}
type alias SoundBite =
  { mss  : Maybe SoundSample
  , time : Float
  , gain : Float
  }

{-| Load an Audio Buffer Sound Sample from a URL -}
loadSoundFont: String -> Signal (Maybe SoundSample)
loadSoundFont name =  Native.SoundFont.loadSoundFont name

{-| Get the audio context's current time -}
getCurrentTime : () -> Float
getCurrentTime = Native.SoundFont.getCurrentTime

{-| play an optional sound sample (if it's there) -}
-- maybePlay : SoundBite -> Task x ()
maybePlay : SoundBite -> Task Never ()
maybePlay sb =
    case sb.mss of 
      Nothing ->
         succeed ()
      Just ss -> 
         Native.SoundFont.play ss.buffer (getCurrentTime() + sb.time) sb.gain
