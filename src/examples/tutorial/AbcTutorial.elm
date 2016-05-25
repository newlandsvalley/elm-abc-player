module AbcTutorial exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick, onInput)
import Html.App as Html
{- import DynamicStyle exposing (hover) -}
import Task exposing (Task, andThen, succeed, sequence, onError)
import Process exposing (sleep)
import List exposing (reverse)
import Maybe exposing (Maybe, withDefault)
import String exposing (toInt)
import Result exposing (Result, formatError)
import Array exposing (Array, get)
import Abc exposing (..)
import AbcPerformance exposing (melodyFromAbcResult)
import Melody exposing (..)
import Notable exposing (..)
import MidiNotes exposing (..)
import Lessons exposing (..)
import Json.Encode as Json
import SoundFont.Ports exposing (..)
import SoundFont.Types exposing (..)

import Debug exposing (..)

main =
  Html.program
    { init = (init, requestLoadFonts "assets/soundfonts"), update = update, view = view, subscriptions = subscriptions }

-- MODEL

type alias Model =
    { fontsLoaded : Bool
    , abc : String
    , playing : Bool
    , lessonIndex : Int
    , duration : Float -- the tune duration in seconds
    , error : Maybe ParseError
    }

init : Model
init =
    { 
       fontsLoaded = False
    ,  abc = example 0
    ,  playing = False
    ,  lessonIndex = 0
    ,  duration = 0.0
    ,  error = Nothing
    }

-- UPDATE

type Msg
    = NoOp   
    | FontsLoaded Bool
    | Abc String
    | Play     
    | PlayStarted Bool    -- response from the player that it's started
    | PlayCompleted       -- the play has completed (we compute the time ourselves)
    | ShowButtons         -- immediately after play has ended
    | Move Bool
    | MoveToEnd Bool
    | Error ParseError

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )

    ShowButtons -> ( {model | playing = False } , Cmd.none )

    FontsLoaded loaded ->
      ( { model | fontsLoaded = loaded }
      , Cmd.none
      )  

    Abc s ->  ( { model | abc = s }, Cmd.none )     

    Play -> playAbc model   

    PlayStarted _ -> (model, (suspend model.duration) )

    PlayCompleted -> ( { model | playing = False }, Cmd.none)   

    Move b ->
      let 
        next =
          case b of 
            True -> Basics.min (model.lessonIndex + 1) (Array.length lessons - 1)
            False -> Basics.max (model.lessonIndex - 1) 0 
      in
        ( { model | lessonIndex = next
          , abc = (example next) 
          , error = Nothing }, Cmd.none ) 

    MoveToEnd b ->
      let 
        next =
          case b of 
            True -> (Array.length lessons - 1)
            False -> 0 
      in
        ( { model | lessonIndex = next
          , abc = (example next)
          , error = Nothing }, Cmd.none ) 

    Error pe ->  ( { model | error = Just pe }, showButtonsAction  ) 


-- SUBSCRIPTIONS
fontsLoadedSub : Sub Msg
fontsLoadedSub  =
  fontsLoaded FontsLoaded

playSequenceStartedSub : Sub Msg
playSequenceStartedSub  =
  playSequenceStarted PlayStarted

subscriptions : Model -> Sub Msg
subscriptions m =
  Sub.batch [fontsLoadedSub, playSequenceStartedSub]

-- COMMANDS

{- sleep for a number of seconds -}
suspend : Float -> Cmd Msg
suspend secs =
  let
    _ = log "suspend time" secs
    time = secs * 1000
  in 
    Process.sleep time
      |> Task.perform (\_ -> NoOp) (\_ -> PlayCompleted)    
    
{- just the ShowButton action wrapped in a Task -}
showButtons : Task Never Msg
showButtons = succeed (ShowButtons)            

{- and as an effect -}
showButtonsAction : Cmd Msg
showButtonsAction =
  Task.perform (\_ -> NoOp) (\_ -> NoOp) showButtons
     
{- calculate the performance duration in seconds -}
performanceDuration : MidiNotes -> Float
performanceDuration notes =
   let
     maybeLastNote = List.head (List.reverse notes)
   in 
     case maybeLastNote of
       Nothing -> 0.0
       Just n -> n.timeOffset  -- the accumulated time

returnError : ParseError -> Cmd Msg
returnError e =
  Task.succeed (Error e)
    |> Task.perform (\_ -> NoOp) (\_ -> NoOp)
    
terminateLine : String -> String
terminateLine s =
  s ++ "|\r\n" 

{- cast a String to an Int -}
toInt : String -> Int
toInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0

{- play the ABC and return the duration in the amended model -}
playAbc : Model -> (Model, Cmd Msg)
playAbc m = 
  let 
    notesReversed = 
      m.abc
        |> terminateLine
        |> parse 
        |> melodyFromAbcResult 
        |> toPerformance
        |> makeMIDINotes
    -- _ = log "notes reversed" notesReversed
    duration =
      reversedPhraseDuration notesReversed
  in 
    ( { m | playing = True
          , duration = duration }, requestPlayNoteSequence (List.reverse notesReversed) )   

-- VIEW

viewError : Maybe ParseError -> String
viewError me =
  case me of
    Nothing -> ""
    Just e -> parseError e 

view : Model -> Html Msg
view model =
  if (model.fontsLoaded) then
    div [ centreStyle ]
      [  
         h1 [ ] [ text "ABC Tutorial" ]   
      ,  h2 [ ] [ text (title model.lessonIndex) ]     
      ,  textarea 
           [
           value  (instruction model.lessonIndex) 
           , instructionStyle
           , readonly True
           , cols 96
           , rows 6
           ]
           [ ]
      ,  fieldset [ fieldsetStyle ]
           [
             legend [ legendStyle ] [ text "you can edit the text inside the box and then hit play" ]
           , textarea
               ([ 
               placeholder "abc"
               , value model.abc
               , onInput Abc 
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
              button ( buttonAttributes model.playing (MoveToEnd False))
                       [ text "first" ]
           ,  button ( buttonAttributes model.playing (Move False))
                       [ text "previous" ]
           ,  button ( buttonAttributes model.playing Play)
                       [ text "play" ]
           ,  button ( buttonAttributes model.playing (Move True))
                       [ text "next" ]
           ,  button ( buttonAttributes model.playing (MoveToEnd True))
                       [ text "last" ]
           ]
      ,  div 
         [ centreStyle ] 
           [ p [] [ text (hint model.lessonIndex) ] 
           , p [] [ text (viewError model.error) ] 
           ]
      ]
  else
    div [ centreStyle ]
      [  p [ ] [ text "It seems as if your browser does not support web-audio.  Perhaps try Chrome" ]
      ] 

title : Int -> String
title i =
  let 
    mlesson = Array.get i lessons
  in case mlesson of
    Nothing -> "error"
    Just l -> "lesson " ++ (toString (i+1) ++ " - " ++ l.title)

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

hint : Int -> String
hint i =
  let 
    mlesson = Array.get i lessons
  in case mlesson of
    Nothing -> ""
    Just l -> l.hint
       


{- style a textarea -}
taStyle : Attribute Msg
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
instructionStyle : Attribute Msg
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
centreStyle : Attribute Msg
centreStyle =
  style
     [
       ("text-align", "center")
     ,  ("margin", "auto") 
     ]

{- gather together all the button attributes -}
buttonAttributes : Bool -> Msg -> List (Attribute Msg)
buttonAttributes isDisabled msg =
  hoverButton isDisabled ++
    [ bStyle isDisabled
    , onClick msg
    , disabled isDisabled 
    ] 

{- style a button -}
bStyle : Bool -> Attribute Msg
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
hoverButton : Bool -> List (Attribute msg)
hoverButton enabled =      
  if enabled then
     {- hover [("background-color","#67d665","#669966")] -}
    []
  else
    []

{- style a fieldset -}
fieldsetStyle : Attribute Msg
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
legendStyle : Attribute Msg
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

highlights : Model -> List (Attribute Msg)
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







