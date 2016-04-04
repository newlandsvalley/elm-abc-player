module AbcEditor where

import Effects exposing (Effects, Never, task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import DynamicStyle exposing (hover)
import Task exposing (Task, andThen, succeed, sequence, onError)
import List exposing (reverse)
import Maybe exposing (Maybe, withDefault)
import String exposing (toInt)
import Result exposing (Result, formatError)
import Array exposing (Array, get)
import Dict exposing (Dict)
import SoundFont exposing (..)
import Abc exposing (..)
import AbcPerformance exposing (melodyFromAbcResult)
import Melody exposing (..)
import Notable exposing (..)
import Debug exposing (..)
import Json.Encode as Json

-- MODEL
type alias Sound = Task Effects.Never ()
type alias Sounds = List Sound

type alias Model =
    { samples : Dict Int SoundSample
    , loaded : Bool
    , maybeContext : Maybe AudioContext
    , abc : String
    , error : Maybe ParseError
    , buttonsDisabled : Bool
    }

init : String -> (Model, Effects Action)
init topic =
  ( { 
       samples = Dict.empty
    ,  loaded = False
    ,  maybeContext = Nothing
    ,  abc = ""
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
               ( finaliseAudioContext model, checkAudio )
            _ -> 
              let pitch = toInt ss.name
              in
                ( { model | samples = Dict.insert pitch ss model.samples }, 
                  Effects.none
                )        

    Abc s ->  ( { model | abc = s }, Effects.none )     

    Play -> ( { model | error = Nothing, buttonsDisabled = True }, playAbc model)   

    Error pe ->  ( { model | error = Just pe }, showButtonsAction  ) 

{- finalise the audio context in the model -}
finaliseAudioContext : Model -> Model
finaliseAudioContext m =
  let
    ctx = 
      if (isWebAudioEnabled) then
        Just (getAudioContext ())
      else
        Nothing
  in
    { m | maybeContext = ctx, loaded = True }


{- inspect the next performance event and generate the appropriate sound command 
   which is done by looking up the sound fonts.  
-}
nextSound : AudioContext -> Dict Int SoundSample -> (Float, Notable) -> Sound
nextSound ctx samples ne = 
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
          maybePlay ctx soundBite
           

{- make the sounds - if we have a performance result from parsing the midi file, convert
   the performance into a list of soundbites (aka Sounds)
-}
makeSounds :  Maybe AudioContext -> Dict Int SoundSample -> Result ParseError Performance -> Sounds 
makeSounds mctx ss perfResult = 
     case perfResult of
       Ok perf ->
         case mctx of
           Just ctx ->
             List.map (nextSound ctx ss) perf
           _ ->
             []
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

{- check Audio is present and show the buttons if so -}
checkAudio : Effects Action
checkAudio =
  if (isWebAudioEnabled) then
    showButtons
      |> Effects.task
  else
    Effects.none
     
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
      makeSounds m.maybeContext m.samples pr
        |> playSounds pr
    Err e ->
      returnError e

-- VIEW

viewError : Maybe ParseError -> String
viewError me =
  case me of
    Nothing -> ""
    Just e -> parseError e 

view : Signal.Address Action -> Model -> Html
view address model =
  if (isWebAudioEnabled) then
    div [ centreStyle ]
      [  
         h1 [ ] [ text "ABC Editor" ]   
      ,  fieldset [ fieldsetStyle ]
           [
             legend [ legendStyle ] [ text "you can edit the text inside the box and then hit play" ]
           , textarea
               ([ 
               placeholder "abc"
               , value model.abc
               , on "input" targetValue (\a -> Signal.message address (Abc a))
               , taStyle
               , cols 70
               , rows 12
               , autocomplete False
               , spellcheck False
               , autofocus True
               ] ++ highlights model)
               [  ] 
           ]
      ,  div
         [ centreStyle ]       
           [  
 
             button ( buttonAttributes model.buttonsDisabled address Play)
                       [ text "play" ]
 
           ]
      ,  div 
         [ centreStyle ] 
           [ 
             p [] [ text (viewError model.error) ] 
           ]
      ]
  else
    div [ centreStyle ]
      [  p [ ] [ text "It seems as if your browser does not support web-audio.  Perhaps try Chrome" ]
      ] 


{- style a textarea -}
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
    , ("background-color",  "#f3f6c6")
    ]


{- style the instructions section -}
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
    , ("font", "100% \"Trebuchet MS\", Verdana, sans-serif")
    ]


{- style a centered component -}    
centreStyle : Attribute
centreStyle =
  style
     [
       ("text-align", "center")
     ,  ("margin", "auto") 
     ]

{- gather together all the button attributes -}
buttonAttributes : Bool -> Signal.Address Action -> Action -> List Attribute
buttonAttributes isDisabled address action =
  hoverButton isDisabled ++
    [ bStyle isDisabled
    , onClick address action
    , disabled isDisabled 
    ] 

{- style a button -}
bStyle : Bool -> Attribute
bStyle disabled = 
  let
    basecss =
      [
        ("border", "none")
      , ("padding", "5px 10px")
      , ("-webkit-border-radius", "8px")
      , ("-moz-border-radius", "8px")
      , ("-webkit-box-shadow", "rgba(0,0,0,1) 0 1px 0")
      , ("-moz-box-shadow", "rgba(0,0,0,1) 0 1px 0")
      , ("box-shadow", "rgba(0,0,0,1) 0 1px 0")
      , ("text-shadow", "rgba(0,0,0,.4) 0 1px 0")
      , ("font-size", "14px")
      , ("font-family", "Georgia, serif")
      , ("text-decoration", "none")
      , ("vertical-align", "middle") 
      , ("margin", "5px 5px 5px 5px")
      , ("font", "100% \"Trebuchet MS\", Verdana, sans-serif")
      , ("-webkit-transition-duration", "0.2s")
      , ("-moz-transition-duration", "0.2s")
      , ("transition-duration", "0.2s")
     ]
    colour =
      if disabled then
        [ ("background-color", "#7D7C7C")
        , ("color", "grey")
        ]
      else
        [ ("background-color", "#67d665")
        , ("background", "-webkit-gradient(linear, left top, left bottom, from(#3e9c5f), to(#67d665))")
        , ("background", "-webkit-linear-gradient(top, #3e9c5f, #67d665)")
        , ("background", "-moz-linear-gradient(top, #3e9c5f, #67d665)")
        , ("background", "-ms-linear-gradient(top, #3e9c5f, #67d665)")
        , ("background", "-o-linear-gradient(top, #3e9c5f, #67d665)")
        , ("color", "black")
        ]  
  in
    style (basecss ++ colour)

{- hover over a button -}
hoverButton : Bool -> List Attribute
hoverButton disabled =      
  if disabled then
    []
  else
    hover [("background-color","#67d665","#669966")]

{- style a fieldset -}
fieldsetStyle : Attribute
fieldsetStyle =
  style 
    [
      ("background-color",  "#f1f1f1")
    , ("border", "none")
    , ("border-radius", "2px")
    , ("margin-bottom", "12px")
    , ("padding", "10px 10px 20px 10px")
    , ("display", "inline-block")
    ]

{- style a fieldset legend -}
legendStyle : Attribute
legendStyle = 
  style
   [
     ("background-color",  "#67d665") 
   , ("border-top", "1px solid #d4d4d4")
   , ("border-bottom", "1px solid #d4d4d4")
   , ("-moz-box-shadow", "3px 3px 3px #ccc")
   , ("-webkit-box-shadow", "3px 3px 3px #ccc")
   , ("box-shadow", "3px 3px 3px #ccc")
   , ("font-size", "1em")
   , ("padding", "0.3em 1em")
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
pianoFonts = loadSoundFont (getAudioContext ()) "acoustic_grand_piano"

signals : List (Signal Action)
signals = 
  if (isWebAudioEnabled) then 
    [Signal.map LoadFont pianoFonts]
  else
    []





