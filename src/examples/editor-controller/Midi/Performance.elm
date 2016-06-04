module Midi.Performance exposing 
   ( Notable (..)
   , NoteEvent
   , MidiPerformance
   , fromRecording) 

{-|  conversion of a MIDI recording to a performance

# Definition

# Data Types
@docs Notable, NoteEvent,  MidiPerformance

# Functions
@docs fromRecording

-}

import MidiTypes exposing (..)
import Maybe exposing (withDefault)

type alias AccumulatedTime = Int

{-| Note descriptions we need to keep-}
type Notable =  MicrosecondsPerBeat Int
              | Note Int Int
              | NoNote

{-| Midi NoteEvent -}    
type alias NoteEvent = (AccumulatedTime, Notable)

{-| Melody Line -}    
type alias Line = List NoteEvent

{-| Midi Performance -}
type alias MidiPerformance = 
    { formatType : Int
    , ticksPerBeat : Int
    , lines : List Line
    }

{-| translate a MIDI recording to a simple performance -}
fromRecording : MidiRecording -> MidiPerformance
fromRecording mr = 
   let 
      header = fst mr
      lines = List.map makeNotes 
              <| List.map accumulateTimes 
              <| snd mr
      linesWithTempo = distributeTempo header.formatType lines
   in 
      { formatType = header.formatType, ticksPerBeat = header.ticksPerBeat, lines = linesWithTempo }

makeNotes : List (Int, MidiEvent) -> List (Int, Notable)
makeNotes = List.map eventToNotable

{- translate a timed MidiEvent to a timed Notable -}
eventToNotable : (Int, MidiEvent) -> (Int, Notable)
eventToNotable (t,e) = case e of 
                    NoteOn chanel pitch velocity -> (t, Note pitch velocity)
                    Tempo x -> (t, MicrosecondsPerBeat x)
                    _ -> (t, NoNote)

{- filter so we only have Tempo and NoteOn messages -}
filterEvents : Track -> Track
filterEvents = List.filter (\(t, e) -> case e of
                                NoteOn _ _ _ -> True
                                Tempo _ -> True
                                _ -> False
                            )

{- keep a running total of accumulated ticks -}
accum : MidiMessage -> List MidiMessage -> List MidiMessage 
accum nxt acc = let at = case acc of 
                      [] -> 0
                      x :: xs -> fst x
                    nt = fst nxt
                    nv = snd nxt
                 in
                    (at + nt, nv) :: acc

{-| accumulate the timings and leave only Tempo and NoteOn messages -}
accumulateTimes : Track -> Track
accumulateTimes = filterEvents << List.reverse << List.foldl accum [] 

{- distribute the tempo if it's a Type-1 or Type-2 recording.
   This is done by taking the head of the list - Track 0 (which may or may not contain a tempo)
   and appending it to each of the following tracks
-}
distributeTempo : Int -> List Line -> List Line
distributeTempo formatType ls =
  case formatType of
    0 -> ls
    _ -> 
      let 
        t0 = List.head ls
             |> withDefault []
        ts = List.tail ls
             |> withDefault []
      in
        List.map (\t -> t0 ++ t) ts
