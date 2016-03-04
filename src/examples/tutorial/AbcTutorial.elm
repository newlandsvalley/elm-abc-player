module AbcTutorial where

import Effects exposing (Effects, Never, task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Task exposing (Task, andThen, succeed, sequence)
import List exposing (reverse)
import Maybe exposing (Maybe, withDefault)
import String exposing (toInt)
import Result exposing (Result, formatError)
import Array exposing (Array, get)
import Dict exposing (Dict)
import SoundFont exposing (..)
import Abc exposing (..)
import AbcPerformance exposing (melodyFromAbcResult)
import Performance exposing (..)
import Notable exposing (..)
import Lessons exposing (..)
import Debug exposing (..)
import Json.Encode as Json

-- MODEL
type alias Sound = Task Effects.Never ()
type alias Sounds = List Sound

type alias Model =
    { samples : Dict Int SoundSample
    , loaded : Bool
    , abc : String
    , lessonIndex : Int
    , error : Maybe ParseError
    , buttonsDisabled : Bool
    }

init : String -> (Model, Effects Action)
init topic =
  ( { 
       samples = Dict.empty
    ,  loaded = False
    ,  abc = example 0
    ,  lessonIndex = 0
    ,  error = Nothing
    ,  buttonsDisabled = True -- disabled until the soundfonts load
    }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp   
    | LoadFont (Maybe SoundSample)
    | Abc String
    | Play     
    | ShowButtons -- immediately after play has ended
    | Move Bool
    | Error ParseError

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none )

    ShowButtons -> ( {model | buttonsDisabled = False } , Effects.none )

    LoadFont mss ->
      case mss of
        Nothing ->
          (model, Effects.none)
        Just ss -> 
          case ss.name of
            "end" ->
               ({ model | loaded = True }, showButtonsAction )
            _ -> 
              let pitch = toInt ss.name
              in
                ( { model | samples = Dict.insert pitch ss model.samples }, 
                  Effects.none
                )        

    Abc s ->  ( { model | abc = s }, Effects.none )     

    Play -> ( { model | error = Nothing, buttonsDisabled = True }, playAbc model)   

    Move b ->
      let 
        next =
          case b of 
            True -> Basics.min (model.lessonIndex + 1) (Array.length lessons - 1)
            False -> Basics.max (model.lessonIndex - 1) 0 
      in
        ( { model | lessonIndex = next, abc = (example next) }, Effects.none ) 

    Error pe ->  ( { model | error = Just pe }, Effects.none ) 


{- inspect the next performance event and generate the appropriate sound command 
   which is done by looking up the sound fonts.  
-}
nextSound : Dict Int SoundSample -> (Float, Notable) -> Sound
nextSound samples ne = 
  let 
    (time, notable) = ne
  in
    case notable of
      -- we've hit a Note
      Note pitch velocity ->
        let 
          sample = Dict.get pitch samples
          soundBite = { mss = sample, time = time, gain = velocity }
        in
          maybePlay soundBite
           

{- make the sounds - if we have a performance result from parsing the midi file, convert
   the performance into a list of soundbites (aka Sounds)
-}
makeSounds :  Dict Int SoundSample -> Result ParseError Performance -> Sounds 
makeSounds ss perfResult = 
     case perfResult of
       Ok perf ->
         List.map (nextSound ss) perf
       Err err ->
         []
 
{- play the sounds as a single uninterruptible task -}
playSounds : Result ParseError Performance -> Sounds -> Effects Action
playSounds rp sounds =   
   playAndSuspend rp sounds
      |> Task.map (\_ -> ShowButtons)
      |> Effects.task      
      
{- play the sounds and suspend the UI -}      
playAndSuspend :  Result ParseError Performance -> Sounds -> Task Never Action
playAndSuspend rp sounds =
   sequence sounds   
    `andThen` (\_ -> suspend rp)      
      
{- sleep for a number od seconds -}
suspend : Result ParseError Performance -> Task Never Action
suspend rp =
  let
    time = performanceDuration rp * 1000
  in 
    Task.sleep time
      `andThen` (\_ -> succeed (NoOp))
    
{- just the ShowButton action wrapped in a Task -}
showButtons : Task Never Action
showButtons = succeed (ShowButtons)            

{- and as an effect -}
showButtonsAction : Effects Action
showButtonsAction =
  showButtons
  |> Effects.task
     
performanceDuration : Result ParseError Performance  -> Float
performanceDuration rp =
   let
      notes = log "performance notes" (Result.withDefault [] rp)
      maybeLastNote = List.head notes
   in 
      case maybeLastNote of
        Nothing -> log "nothing" 0.0
        Just ne -> log "Just" (fst ne)  -- the accumulated time

returnError : ParseError -> Effects Action
returnError e =
  Task.succeed (Error e)
    |> Effects.task
    
terminateLine : String -> String
terminateLine s =
  s ++ "\r\n" 

{- cast a String to an Int -}
toInt : String -> Int
toInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0

{- note melody is reversed -}
toPerformance : Result ParseError MelodyLine -> Result ParseError Performance
toPerformance ml = 
   let 
     melody = log "melody" ml
   in
     Result.map (fromMelodyLine 0.0) melody

    
playAbc : Model -> Effects Action
playAbc m = 
  let pr = 
    m.abc
      |> terminateLine
      |> parse 
      |> melodyFromAbcResult 
      |> toPerformance
  in case pr of
    Ok _ ->
      makeSounds m.samples pr
        |> playSounds pr
    Err e ->
      returnError e

title : Int -> String
title i =
  let 
    mlesson = Array.get i lessons
  in case mlesson of
    Nothing -> "error"
    Just l -> (toString (i+1) ++ " - " ++ l.title)

instruction : Int -> String
instruction i =
  let 
    mlesson = Array.get i lessons
  in case mlesson of
    Nothing -> "error"
    Just l -> l.instruction

example : Int -> String
example i =
  let 
    mlesson = Array.get i lessons
  in case mlesson of
    Nothing -> "error"
    Just l -> l.example
       

-- VIEW

viewError : Maybe ParseError -> String
viewError me =
  case me of
    Nothing -> ""
    Just e -> parseError e 

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [  
       h1 [ bStyle ] [ text (title model.lessonIndex) ]     
    ,  textarea 
         [
         value  (instruction model.lessonIndex) 
         , instructionStyle
         , readonly True
         , cols 100
         , rows 5
         ]
         [ ]
    ,  textarea
         ([ 
         placeholder "abc"
         , value model.abc
         , on "input" targetValue (\a -> Signal.message address (Abc a))
         , taStyle
         , cols 66
         , rows 12
         , autocomplete False
         , spellcheck False
         , autofocus True
         ] ++ highlights model)
         [  ] 
    ,  div
       [ bStyle  ]          
         [  button [ onClick address (Move False), disabled model.buttonsDisabled ] [ text "previous" ]
         ,  button [ onClick address Play, disabled model.buttonsDisabled  ] [ text "play" ]
         ,  button [ onClick address (Move True), disabled model.buttonsDisabled  ] [ text "next" ]
         ]
    ,  div 
       [ bStyle ] 
         [ p [] [ text (viewError model.error) ] ]
    ]

taStyle : Attribute
taStyle =
  style
    [
      ("padding", "10px 0")
    , ("font-size", "1.5em")
    , ("text-align", "left")
    , ("align", "center")
    , ("display", "block")
    , ("margin-left", "auto")
    , ("margin-right", "auto")
    ]

instructionStyle : Attribute
instructionStyle =
  style
    [
      ("padding", "10px 0")
    , ("border", "none")
    , ("text-align", "left")
    , ("align", "center")
    , ("display", "block")
    , ("margin-left", "auto")
    , ("margin-right", "auto")
    ]
    
bStyle : Attribute
bStyle =
  style
     [
       ("text-align", "center")
     ,  ("margin", "auto") 
     ]

highlights : Model -> List Attribute
highlights model =
  let 
    mpe = model.error
  in
    case mpe of
      Nothing ->
        []
      Just pe ->
       if (String.length model.abc > pe.position) then
         [ property "selectionStart" (Json.string (toString pe.position))
         , property "selectionEnd" (Json.string (toString (pe.position  + 1)))
         , property "focus" (Json.null)
         ]
       else
         []

-- INPUTS

-- try to load the entire piano soundfont
pianoFonts : Signal (Maybe SoundSample)
pianoFonts = loadSoundFont  "acoustic_grand_piano"

signals : List (Signal Action)
signals = [Signal.map LoadFont pianoFonts]





