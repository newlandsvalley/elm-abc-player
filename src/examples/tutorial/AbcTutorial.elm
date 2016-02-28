module AbcTutorial where

import Effects exposing (Effects, task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import Array exposing (Array, get)
import Dict exposing (Dict)
import SoundFont exposing (..)
import Abc exposing (..)
import Music.Notation exposing (..)
import AbcPerformance exposing (..)
import Performance exposing (..)
import Repeats exposing (..)
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
    , performance : Result String Performance
    , lessonIndex : Int
    , error : Maybe ParseError
    }

init : String -> (Model, Effects Action)
init topic =
  ( { 
       samples = Dict.empty
    ,  loaded = False
    ,  abc = example 0
    ,  performance = Err "not started"
    ,  lessonIndex = 0
    ,  error = Nothing
    }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp   
    | LoadFont (Maybe SoundSample)
    | Abc String
    | Play
    | Move Bool
    | Error ParseError

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none )

    LoadFont mss ->
      case mss of
        Nothing ->
          (model, Effects.none)
        Just ss -> 
          case ss.name of
            "end" ->
               ({ model | loaded = True }, Effects.none )
            _ -> 
              let pitch = toInt ss.name
              in
                ( { model | samples = Dict.insert pitch ss model.samples }, 
                  Effects.none
                )        

    Abc s ->  ( { model | abc = s }, Effects.none )     

    Play -> ( { model | error = Nothing }, playAbc model)   

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
playSounds : Sounds -> Effects Action
playSounds sounds = 
      sequence sounds
      |> Task.map (\x -> NoOp)
      |> Effects.task

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
        |> playSounds
    Err e ->
      returnError e

title : Int -> String
title i =
  let 
    mlesson = Array.get i lessons
  in case mlesson of
    Nothing -> "error"
    Just l -> l.title

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
         , cols 50
         , rows 12
         , autocomplete False
         , spellcheck False
         , autofocus True
         ] ++ highlights model)
         [  ] 
    ,  div
       [ bStyle ]          
         [  button [ onClick address (Move False) ] [ text "previous" ]
         ,  button [ onClick address Play ] [ text "play" ]
         ,  button [ onClick address (Move True) ] [ text "next" ]
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
    , ("font-size", "2em")
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
    , ("margin", "auto") 
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





