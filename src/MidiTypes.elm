module MidiTypes exposing
    ( Track
    , Header
    , MidiEvent(..)
    , MidiMessage
    , MidiRecording
    ) 

{-|  Type Definition of a MIDI recording

# Definition

# Data Types
@docs Header, Track, MidiEvent, MidiMessage,  MidiRecording

-}

type alias Ticks = Int

{-| Midi Event -}
type MidiEvent = -- meta messages
                  SequenceNumber Int
                | Text String
                | Copyright String
                | TrackName String
                | InstrumentName String
                | Lyrics String
                | Marker String
                | CuePoint String
                | ChannelPrefix Int
                | Tempo Int
                | SMPTEOffset Int Int Int Int Int
                | TimeSignature Int Int Int Int
                | KeySignature Int Int
                | SequencerSpecific String
                | SysEx String
                | Unspecified Int (List Int)
                 -- channel messages
                | NoteOn Int Int Int
                | NoteOff Int Int Int
                | NoteAfterTouch Int Int Int
                | ControlChange Int Int Int
                | ProgramChange Int Int
                | ChannelAfterTouch Int Int 
                | PitchBend Int Int
                | RunningStatus Int Int

{-| Midi Message -}    
type alias MidiMessage = (Ticks, MidiEvent)

{-| Midi Track -}
type alias Track = List MidiMessage

{-| Midi Header -}
type alias Header = 
   { formatType : Int
   , trackCount : Int
   , ticksPerBeat : Int
   }

{-| Midi Recording -}
type alias MidiRecording = (Header, List Track)


