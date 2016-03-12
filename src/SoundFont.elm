module SoundFont
    (  AudioContext
      ,SoundSample
      ,SoundBite
      ,isWebAudioEnabled
      ,getAudioContext
      ,loadSoundFont
      ,getCurrentTime
      ,maybePlay
    ) where

{-|  Library for working with the WebAudio API using SoundFonts,

# Definition

# Data Types
@docs AudioContext, SoundSample, SoundBite

# Functions
@docs isWebAudioEnabled, getAudioContext, loadSoundFont, getCurrentTime, maybePlay

-}

import Native.SoundFont
import Task exposing (Task, andThen, mapError, succeed)
import Maybe exposing (Maybe)

{-| Audio Buffers -}
type AudioBuffer = AudioBuffer
{-| Audio Context -}
type AudioContext = AudioContext

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

{-| is the browser web-audio enabled ? -}
isWebAudioEnabled : Bool
isWebAudioEnabled = Native.SoundFont.isWebAudioEnabled()

{-| Get the audio context -}
getAudioContext : () -> AudioContext 
getAudioContext = (\_ -> Native.SoundFont.getAudioContext())

{-| Load an Audio Buffer Sound Sample from a URL -}
loadSoundFont: AudioContext -> String -> Signal (Maybe SoundSample)
loadSoundFont ctx name =  Native.SoundFont.loadSoundFont ctx name

{-| Get the audio context's current time -}
getCurrentTime : AudioContext -> Float
getCurrentTime ctx = Native.SoundFont.getCurrentTime ctx

{-| play an optional sound sample (if it's there) -}
maybePlay : AudioContext -> SoundBite -> Task x ()
maybePlay ctx sb =
    case sb.mss of 
      Nothing ->
         succeed ()
      Just ss -> 
         Native.SoundFont.play ctx ss.buffer (getCurrentTime(ctx) + sb.time) sb.gain
