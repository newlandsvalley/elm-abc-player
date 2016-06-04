module Midi.Player exposing ( Model, Msg (SetRecording), init, update, view, subscriptions )

{-
  A Midi Player module

  This allows buttons of start/pause/continue/reset

  in order to contol the playing of the MIDI file
  (again played by means of soundfonts and Web-Audio through elm ports)

-}

import Html exposing (Html, div, button, input, text, progress)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src, type', style, value, max)
import Http exposing (..)
import Html.App as Html
import Task exposing (..)
import Array exposing (get)
import String exposing (..)
import Result exposing (Result)
import Process exposing (sleep)
import MidiTypes exposing (MidiEvent(..), MidiRecording)
import SoundFont.Ports exposing (..)
import SoundFont.Types exposing (..)
import Midi.Track exposing (..)

import Debug exposing (..) 

main =
  Html.program
    { init = init (Err "not started"), update = update, view = view, subscriptions = subscriptions }

-- MODEL

--  a delta time measured in milliseconds and a MIDI event
type alias SoundEvent = 
   { deltaTime: Float
   , event : MidiEvent
   }

{-| the current state of the playback -}
type alias PlaybackState =
   { index : Int                    -- index into the MidiMessage Array
   , microsecondsPerBeat : Float    -- current Tempo
   , playing : Bool                 -- are we currently playing?
   , noteOnSequence : Bool          -- are we in the midst of a NoteOn sequence
   , noteOnChannel : Int            -- if so, what's its channel
   }

{-| the model of the player -}
type alias Model =
    { 
      fontsLoaded : Bool                    -- are the fonts loaded
    , track : Result String MidiTrack       -- the midi recording to play
    , playbackState : PlaybackState         -- the state of the playback
    }

{-the slowdown in the player brought about by using elm's Tasks -}
elmPlayerOverhead : Float
elmPlayerOverhead = 0.872
-- elmPlayerOverhead = 0.923
    
-- let's use this to mark the end of a track or a track in error we can't play
endOfTrack : MidiTypes.MidiEvent
endOfTrack = MidiTypes.Text "EndOfTrack"

{-| initialise the model and issue a command to load the sound fonts -}
init : Result String MidiTrack -> (Model, Cmd Msg)
init track =
  { 
    fontsLoaded = False
  , track = track
  , playbackState = { index = 0 
                    , microsecondsPerBeat = Basics.toFloat 500000
                    , playing = False 
                    , noteOnSequence = False
                    , noteOnChannel = -1
                    }
  } ! [requestLoadFonts "assets/soundfonts"]

-- UPDATE

{-| the messages used by the player -}
type Msg
    = NoOp   
    | FontsLoaded Bool                           -- response that soundfonts have been loaded
    | SetRecording (Result String MidiRecording) -- an external command to set the recording that is to be played
    | Step                                       -- step to the next event in the MIDI recording
    | Play MidiNote                              -- request to play a single note
    | PlayedNote Bool                            -- response that the note has been played
    -- controller actions
    | Start                                      -- start / restart
    | Pause                                      -- pause
    | MoveTo Int                                 -- move to index (usually invoked as move to start)

{-| update the player -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )   

    FontsLoaded loaded ->
      ( { model | fontsLoaded = loaded }
      , Cmd.none
      )  
   
    SetRecording r ->
        let
           state =  model.playbackState 
           newState = { state | playing = False 
                              , noteOnSequence = False
                              , noteOnChannel = -1 
                      }
           newModel = { model | playbackState = newState
                              , track = toTrack0 r
                      }
        in
          (newModel, stop)  

    Start ->
        -- chaining the next action which is step
        let 
           state =  model.playbackState 
           newState = { state | playing = True }
           newModel = { model | playbackState = newState }
           cmd = step 0.0
        in        
          (newModel, cmd)

    Pause ->   
       let  
         state =  model.playbackState 
         newState = { state | playing = False }
         newModel = { model | playbackState = newState }
       in
         (newModel, Cmd.none)  

    MoveTo index ->
        let 
           state =  model.playbackState 
           newState = { state | playing = False, index=index }
           newModel = { model | playbackState = newState }
        in        
          (newModel, Cmd.none)       

    Step -> 
        let 
          _ = log "step state" model.playbackState
          soundEvent = nextEvent model.playbackState model.track 
          (newState, maybeNote) = stepState soundEvent model.playbackState
          -- next action is either suspendAndPlay or step
          nextAction = interpretSoundEvent soundEvent maybeNote newState
          newModel = { model | playbackState = newState }
        in
          (newModel, nextAction) 

    Play note ->   
      (model, play note)

    PlayedNote played ->
      (model, step 0.0 )

{- extract track zero from the midi recording -}
toTrack0 : Result String MidiRecording -> Result String MidiTrack
toTrack0 r = 
  Result.map fromRecording r


-- SUBSCRIPTIONS
fontsLoadedSub : Sub Msg
fontsLoadedSub  =
  fontsLoaded FontsLoaded

playedNoteSub : Sub Msg
playedNoteSub   =
  playedNote PlayedNote

subscriptions : Model -> Sub Msg
subscriptions m =
  Sub.batch [fontsLoadedSub, playedNoteSub]
      
-- EFFECTS


{- get the next event - if we have a recording result from parsing the midi file, convert
   the next indexed midi event to a delayed action (perhaps a NoteOn sound)
-}
nextEvent :  PlaybackState -> Result String MidiTrack -> SoundEvent
nextEvent state trackResult = 
     case trackResult of
       Ok track ->
        let 
          maybeNextMessage = track.messages
                             |> Array.get state.index
          nextMessage = Maybe.withDefault (0, endOfTrack ) maybeNextMessage
          nextEvent = snd nextMessage     
          -- work out the interval to the next note in milliseconds       
          deltaTime = fst nextMessage * state.microsecondsPerBeat  / (Basics.toFloat track.ticksPerBeat  * 1000)
          {-
          _ = log "midi note delay" (fst nextMessage)
          _ = log "delta time" deltaTime
          -}
        in
          { deltaTime = deltaTime, event = nextEvent }
       Err err ->
          { deltaTime = 0.0, event = endOfTrack }

          

{- interpret the sound event - delay for the specified time and play the note if it's a NoteOn event
   otherwise just step to the next MIDI event
 -}
interpretSoundEvent : SoundEvent -> Maybe MidiNote -> PlaybackState -> Cmd Msg
interpretSoundEvent soundEvent maybeNote state = 
    if (state.playing) then
      case maybeNote of
        Just note ->
           suspendAndPlay (soundEvent.deltaTime * elmPlayerOverhead) note
        _ ->
           step (soundEvent.deltaTime * elmPlayerOverhead)
    else
      Cmd.none

{- a note is played by sleeping for its time offset and then requesting
   that the note be played (through the port)
-}
suspendAndPlay : Float -> MidiNote -> Cmd Msg
suspendAndPlay delay note =
  Process.sleep ( delay )
    |> Task.perform (\_ -> NoOp) (\_ -> Play note)

{- a non-note is processed by sleeping for the time delay and then
   stepping to the next MIDI event
-} 
step : Float -> Cmd Msg
step delay =
  let
    task = 
       Process.sleep ( delay )
         `andThen` (\_ -> Task.succeed (\_ -> Step))
  in
    Task.perform (\_ -> NoOp) (\_ -> Step) task

{- play a note -}
play : MidiNote -> Cmd Msg
play note =
  let 
    note1 = { note | timeOffset = 0.0 }
  in
    requestPlayNote note1

{- issue a stop asynchronously to what the player thinks it's doing - this is issued
   externally whenever the player gets a new MIDI recording to play and must interrupt
   what it's doing.
-}
stop : Cmd Msg
stop = 
  Task.perform (\_ -> NoOp) (\_ -> MoveTo 0) (Task.succeed NoOp)

{- step through the state, and return the note if it's a NoteOn message
   if it's a RunningStatus message, then step to a note as if the previous 
   message was NoteOn 
-}
stepState : SoundEvent -> PlaybackState -> (PlaybackState, Maybe MidiNote)
stepState soundEvent state = 
  if state.playing then
    let 
      _ = log "sound event" soundEvent.event
    in
      case soundEvent.event of
        MidiTypes.Text t ->
          if (t == "EndOfTrack") then      
            ({ state | playing = False, noteOnSequence = False }, Nothing)
          else 
            ({ state | index = state.index + 1, noteOnSequence = False}, Nothing)

        Tempo t -> 
          ( { state | microsecondsPerBeat = Basics.toFloat t, index = state.index + 1, noteOnSequence = False}, Nothing)
     
        {- Running Status inherits the channel from the last event but only (in our case)
           if the state shows we're in the midst of a NoteOn sequence (i.e. a NoteOn followed 
           immediately by 0 or more RunningStatus) then we generate a new NoteOn
        -}
        RunningStatus p1 p2 ->
          if state.noteOnSequence then
            let 
              newEvent = { deltaTime = soundEvent.deltaTime, event = NoteOn state.noteOnChannel p1 p2 }
            in
              stepState newEvent state
          else
            -- ignore anything else and reset the sequence state
            ({ state | index = state.index + 1, noteOnSequence = False} , Nothing)
      
        NoteOn channel pitch velocity ->
          let
            midiNote = (MidiNote pitch soundEvent.deltaTime gain)
            newstate = 
             { state | index = state.index + 1
                     , noteOnSequence = True
                     , noteOnChannel = channel
             }
            maxVelocity = 0x7F
            gain =
              Basics.toFloat velocity / maxVelocity
          in
            (newstate, Just midiNote)
              
        _  -> 
          ({ state | index = state.index + 1, noteOnSequence = False} , Nothing)
     else
       (state, Nothing)

-- VIEW

{- view the result - just for debug purposes -}
viewRecordingResult : Result String MidiTrack -> String
viewRecordingResult mr = 
   case mr of
      Ok res -> 
         "OK: " ++ (toString res)
      Err errs -> 
         "Fail: " ++ (toString errs)

{-| view the player widget -}
view : Model -> Html Msg
view model =
  div [] 
    [  
      player model
    ]

  
player : Model -> Html Msg
player model =
  let
    _ = log "Midi Player view" model.track 
    start = "assets/images/play.png"
    stop  = "assets/images/stop.png"
    pause = "assets/images/pause.png"
    maxRange = case model.track of
      Ok track -> Array.length track.messages |> toString
      _ -> "0"
    sliderPos = model.playbackState.index |> toString
    playButton = 
      case model.playbackState.playing of
        True -> 
          pause
        False ->
          start
    playAction = 
      case model.playbackState.playing of
        True -> 
          Pause
        False ->
          Start
    in
      case model.track of
        Ok _ ->
          div [ style playerBlock ]
            [ div [ style (playerBase ++ playerStyle) ]
              [ progress [ Html.Attributes.max maxRange
                         , value sliderPos 
                         , style capsuleStyle
                         ] [] 
              , div [ style buttonStyle ] 
                [ input [ type' "image"
                        , src playButton
                        , onClick (playAction) 
                        ] [ ]
                , input [ type' "image"
                        , src stop
                        , onClick (MoveTo 0) 
                        ] [ ]
                ]
              ]
           ]
        Err _ ->
         div [] [ text "no midi track present" ]

{- the player buttons -}
buttons : Model -> Html Msg
buttons model =
  case model.playbackState.playing of
    True ->  
      div []
        [ button [ onClick (Pause) ] [ text "pause" ]
        , button [ onClick (MoveTo 0) ] [ text "stop" ]
        ]
    False -> 
      div []
        [ button [ onClick (Start) ] [ text "play" ] 
        , button [ onClick (MoveTo 0) ] [ text "stop" ]
        ]

-- CSS
{- Only half-successful attempt to reuse the styling of the MIDI.js player on which this project is based
   I've lost access to identicalsnowflake/elm-dynamic-style for effects like hover which is no longer
   compatible with Elm 0.16 and my gradient effects don't seem to work.  Not sure what the future
   holds for libraries such as elm-style or elm-css. 
-}
playerBlock : List (String, String)
playerBlock =
  [ ("border", "1px solid #000")
  --, ("background", "#000")
  , ("border-radius", "10px")
  , ("width", "360px")
  , ("position", "relative; z-index: 2")
  -- , ("margin-bottom", "15px")
  ]


playerStyle : List (String, String)
playerStyle =
  [ ("height", "30px")
  , ("box-shadow", "-1px #000")
  , ("border-bottom-right-radius", "10")
  , ("border-bottom-left-radius", "10")
  --, ("margin-bottom", "0" )
  ]

playerBase : List (String, String)
playerBase =
  [ ("background", "rgba(0,0,0,0.7)")
    -- ("background", "#000")
  , ("background-image", "-webkit-gradient(linear,left top,left bottom,from(rgba(66,66,66,1)),to(rgba(22,22,22,1)))")
  , ("background-image", "-webkit-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)")
  , ("background-image", "-moz-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)")
  , ("background-image", "-ms-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)")
  , ("background-image", "-o-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)")
  , ("background-image", "linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)")
  , ("padding", "15px 20px")
  , ("border", "1px solid #000")
  , ("box-shadow", "0 0 10px #fff")
  , ("-moz-box-shadow", "0 0 10px #fff")
  , ("-webkit-box-shadow", "0 0 10px #fff")
  , ("border-radius", "10px")
  , ("-moz-border-radius", "10px")
  , ("-webkit-border-radius", "10px")
  , ("color", "#FFFFFF")
  , ("color", "rgba(255, 255, 255, 0.8)")
  , ("text-shadow", "1px 1px 2px #000")
  , ("-moz-text-shadow", "1px 1px 2px #000")
  -- , ("margin-bottom", "15px")
  ]

buttonStyle : List (String, String)
buttonStyle = 
  [ ("margin", "0 auto")
  , ("width", "80px")
  , ("float", "right")
  , ("opacity", "0.7")
  ]

capsuleStyle : List (String, String)
capsuleStyle = 
  [ ("border", "1px solid #000")
  , ("box-shadow", "0 0 10px #555")
  , ("-moz-box-shadow", "0 0 10px #555")
  , ("-webkit-box-shadow", "0 0 10px #555")
  , ("background", "#000")
  , ("background-image", "-webkit-gradient(linear, left top, left bottom, color-stop(1, rgba(0,0,0,0.5)), color-stop(0, #333))")
  , ("background-image", "-webkit-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)")
  , ("background-image", "-moz-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)")
  , ("background-image", "-ms-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)")
  , ("background-image", "-o-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)")
  , ("background-image", "linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)")
  , ("overflow", "hidden")
  , ("border-radius", "5px")
  , ("-moz-border-radius", "5px")
  , ("-webkit-border-radius", "5px")
  , ("width", "220px")
  , ("display", "inline-block")
  , ("height", "30px")
  ]
   
  







