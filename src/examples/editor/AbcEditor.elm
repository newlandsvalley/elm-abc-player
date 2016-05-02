module AbcEditor where

import Effects exposing (Effects, Never, task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import DynamicStyle exposing (hover)
import Task exposing (Task, andThen, succeed, sequence, onError)
import List exposing (reverse, isEmpty)
import Maybe exposing (Maybe, withDefault)
import String exposing (toInt, slice)
import Result exposing (Result, formatError)
import Array exposing (Array, get)
import Dict exposing (Dict)
import SoundFont exposing (..)
import Abc exposing (..)
import AbcPerformance exposing (melodyFromAbcResult)
import Abc.ParseTree exposing (AbcTune, PitchClass (..), Mode (..), Accidental (..), ModifiedKeySignature, KeySignature)
import Abc.Canonical exposing (fromResult, fromTune)
import Music.Notation exposing (getKeySig)
import Music.Transposition exposing (transposeTo)
import Music.Octave exposing (up, down)
import Melody exposing (..)
import Notable exposing (..)
import Debug exposing (..)
import Json.Encode as Json

{-| Beginnings of an ABC editor.  The eventual plan is to provide an editor which continually parses the ABC as it is entered and flags up errors.
    If the (checked) tune contains a key signature, then transposition options will be shown. 

-}

-- MODEL
type alias Sound = Task Effects.Never ()
type alias Sounds = List Sound

type alias Model =
    { samples : Dict Int SoundSample
    , loaded : Bool
    , playing : Bool
    , maybeContext : Maybe AudioContext
    , abc : String
    , tuneResult : Result ParseError AbcTune
    }

dummyError : ParseError
dummyError = 
  {  msgs = []
  ,  input = ""
  ,  position = 0
  }

emptyTune : AbcTune
emptyTune =
  ([], [])


init : String -> (Model, Effects Action)
init topic =
  ( { 
       samples = Dict.empty
    ,  loaded = False
    ,  playing = False
    ,  maybeContext = Nothing
    ,  abc = ""
    ,  tuneResult = Ok emptyTune
    }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp   
    | LoadFont (Maybe SoundSample)
    | Abc String
    | Play     
    | PlayCompleted    
    | Transpose String
    | MoveOctave Bool
    | TuneResult (Result ParseError AbcTune)

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
               ( finaliseAudioContext model, Effects.none )
            _ -> 
              let pitch = toInt ss.name
              in
                ( { model | samples = Dict.insert pitch ss model.samples }, 
                  Effects.none
                )        

    Abc s ->  ( { model | abc = s }, checkAbc s )     

    Play -> ( { model | playing = True }, playAbc model)   

    PlayCompleted -> ( { model | playing = False }, Effects.none)   

    Transpose s -> (transpose s model, Effects.none )

    MoveOctave isUp -> 
      if isUp then 
        (moveOctave up model, Effects.none )
      else
        (moveOctave down model, Effects.none )

    TuneResult tr ->  ( { model | tuneResult = tr }, Effects.none) 

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
        |> Task.map (\_ -> PlayCompleted)
        |> Effects.task      
      
{- play the sounds and suspend the UI -}      
playAndSuspend :  Result ParseError Performance -> Sounds -> Task Never Action
playAndSuspend rp sounds =
   sequence sounds   
     `andThen` (\_ -> suspend rp)      
      
{- sleep for a number of seconds -}
suspend : Result ParseError Performance -> Task Never Action
suspend rp =
  let
    time = performanceDuration rp * 1000
  in 
    Task.sleep time
      `andThen` (\_ -> succeed (NoOp))
    

{- a different attempt at checking if buttons are enabled -}
areButtonsEnabled : Model -> Bool
areButtonsEnabled m =
  case m.tuneResult of
    Ok _ -> 
      not (m.playing)
    Err _ -> False

{- check Audio is present and show the buttons if so -}
{-
checkAudio : Effects Action
checkAudio =
  if (isWebAudioEnabled) then
    showButtons
      |> Effects.task
  else
    Effects.none
-}
     
performanceDuration : Result ParseError Performance  -> Float
performanceDuration rp =
   let
      notes = log "performance notes" (Result.withDefault [] rp)
      maybeLastNote = List.head notes
   in 
      case maybeLastNote of
        Nothing -> 0.0
        Just ne -> (fst ne)  -- the accumulated time

returnTuneResult : Result ParseError AbcTune -> Effects Action
returnTuneResult r =
  Task.succeed (TuneResult r)
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

checkAbc : String -> Effects Action
checkAbc abc = 
  let 
    terminatedAbc = terminateLine abc
    -- _ = "checking" terminatedAbc
    pr = parse terminatedAbc
  in 
    returnTuneResult (pr)
    
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
      Effects.none

{- transpose the tune to a new key -}
transpose : String -> Model -> Model
transpose kstr model =
  let
    mksr = parseKeySignature kstr
  in
    case (mksr, model.tuneResult) of
      (Ok mks, Ok tune) ->
        let
          newTuneResult = transposeTo mks tune
          -- this is awkward in elm's Result - in this instance we're guaranteed not to have errors
          -- in transposition because our modes always match.  Just convert the notional String error to a notional empty parser error
          newTRCorrectedErr = 
            newTuneResult
               |> formatError (\_ -> dummyError)
          -- and collect the new ABC wrapped in a Result 
          newAbcResult = fromResult newTuneResult
        in
          -- if we're OK, we have both a new ABC Tune and a new ABC source of that tune
          case newAbcResult of
            Ok newAbc ->
              { model | abc = newAbc, tuneResult =  newTRCorrectedErr }
            _ -> model
      _ -> model

{- move the tune up or down an octave -}
moveOctave : (AbcTune -> AbcTune) -> Model -> Model
moveOctave movefn model =
   case model.tuneResult of
     Ok tune ->
       let
         newTune = movefn tune
         newAbc = fromTune newTune
       in 
        { model | abc = newAbc, tuneResult =  (Ok newTune) }            
     _ -> model
      

-- VIEW

viewError : Model -> Html
viewError m =
  let 
    tuneResult = m.tuneResult
  in
    case tuneResult of
      Err e -> 
        -- we start off with a dummy error message which is empty
        if (isEmpty e.msgs) then
          text ""
        else
          let
            -- display a prefix of 5 characters before the error (if they're there) and a suffix of 5 after
            startPhrase = Basics.max (e.position - 5) 0
            errorPrefix = "error: " ++ slice startPhrase e.position m.abc
            startSuffix = Basics.min (e.position + 1) (String.length m.abc)
            endSuffix = Basics.min (e.position + 6) (String.length m.abc)
            errorSuffix = slice startSuffix endSuffix m.abc
            errorChar = slice e.position (e.position + 1) m.abc
          in
            p [ ]
                [ text errorPrefix
                , span [ errorHighlightStyle ]
                    [ text errorChar ]
                , text errorSuffix
                ]
      _ -> text ""

view : Signal.Address Action -> Model -> Html
view address model =
  if (isWebAudioEnabled) then
    div [ ]
      [  
         h1 [ centreStyle ] [ text "ABC Editor" ]   
      ,  div [ leftPaneStyle ]
           [ span [ leftPanelWidgetStyle ] [text "Transpose to:"]
           , transpositionMenu address model 
           , span [ leftPanelWidgetStyle ] [text "Move octave:"]     
           , button ( buttonAttributes (areButtonsEnabled model) address (MoveOctave True))
                       [ text "up" ]   
           , button ( buttonAttributes (areButtonsEnabled model) address (MoveOctave False))
                       [ text "down" ] 
           ]
      ,  div [ rightPaneStyle ]
         [
           fieldset [ fieldsetStyle  ]
             [
            
             textarea
               [ 
               placeholder "abc"
               , value model.abc
               , on "input" targetValue (\a -> Signal.message address (Abc a))
               , taStyle
               , cols 70
               , rows 16
               , autocomplete False
               , spellcheck False
               , autofocus True
               ]  
               [  ] 
            ]
           ,  div
             [  ]       
               [  
 
                 button ( buttonAttributes (areButtonsEnabled model) address Play)
                       [ text "play" ] 
 
               ]
           ,  div 
             [  ] 
               [ 
                 p [] [ viewError model ] 
               ]
           ]
      ]
  else
    div [ centreStyle ]
      [  p [ ] [ text "It seems as if your browser does not support web-audio.  Perhaps try Chrome." ]
      ] 

{- an active menu of transposition options -} 
transpositionMenu : Signal.Address Action -> Model -> Html
transpositionMenu address m =
  let mKeySig =
    case
      m.tuneResult of
        Ok tune -> getKeySig tune
        _ -> Nothing
  in
    case mKeySig of
      Just mks ->
        select [ leftPanelWidgetStyle
               , (disabled m.playing)
               , on "change" targetValue (\a -> Signal.message address (Transpose a)) ] 
          (transpositionOptions mks)
      Nothing -> 
        select [ leftPanelWidgetStyle
               , (disabled True) 
               ] 
          [
            option [] [text "not available" ]
          ]

{- offer a menu of transposition options, appropriate to the 
   current key (if such a key has been entered in the ABC).
   The mode of each option always matches the current mode and 
   the selected option matches the current key
-}
transpositionOptions : ModifiedKeySignature -> List Html
transpositionOptions mks =
  let
    ks = fst mks
    mode = ks.mode    
    allModes =    
      [ option [ selectedKey ks (key C mode) ] 
        [ displayKeySig (key C mode) ]
      , option [ selectedKey ks (key D mode) ] 
        [ displayKeySig (key D mode) ]
      , option [ selectedKey ks (key E mode) ] 
        [ displayKeySig (key E mode) ]
      , option [ selectedKey ks (key F mode) ] 
        [ displayKeySig (key F mode) ]
      , option [ selectedKey ks (key G mode) ] 
        [ displayKeySig (key G mode) ]
      , option [ selectedKey ks (key A mode) ] 
        [ displayKeySig (key A mode) ]
      , option [ selectedKey ks (key B mode) ] 
        [ displayKeySig (key B mode) ]
      ]  
    majorMode = 
      [ option [ selectedKey ks (flatKey B Major) ] 
        [ displayKeySig (flatKey B Major) ]
      , option [ selectedKey ks (flatKey A Major) ] 
        [ displayKeySig (flatKey A Major) ]
      , option [ selectedKey ks (flatKey E Major) ]
        [ displayKeySig (flatKey E Major) ]
      ] 
    minorMode =      
      [ option [ selectedKey ks (sharpKey F Minor) ]
        [ displayKeySig (sharpKey F Minor) ]
      , option [ selectedKey ks (sharpKey C Minor) ] 
        [ displayKeySig (sharpKey C Minor) ]
      , option [ selectedKey ks (sharpKey G Minor) ] 
        [ displayKeySig (sharpKey G Minor) ]
      ]
  in
    case mode of
      Major -> allModes ++ majorMode
      Minor -> allModes ++ minorMode
      _ -> allModes


 

{- return a (selected true) attriubute if the pattern key signature matches the target -}
selectedKey : KeySignature -> KeySignature -> Attribute
selectedKey target pattern =
  let
    isMatched = (target.pitchClass == pattern.pitchClass) && (target.accidental == pattern.accidental)
  in
    selected isMatched

{- display a key signature as text -}
displayKeySig : KeySignature -> Html
displayKeySig ks =
  let 
    accidental =
      case ks.accidental of
        Just Sharp -> "#"
        Just Flat -> "b"
        _ -> ""
  in
    text ( toString ks.pitchClass ++ accidental ++ " " ++ toString ks.mode )


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

leftPanelWidgetStyle : Attribute
leftPanelWidgetStyle =
  style
    [      
      ("margin-left", "40px")
    , ("margin-top", "40px")
    ]

{-
leftPanelWidgetStyle : Attribute
leftPanelWidgetStyle =
  style
    [  
    {-    
      ("margin-left", "40px")
    , ("margin-top", "40px")
    -}
      ("float", "left")
    ]
-}

{- style a centered component -}    
centreStyle : Attribute
centreStyle =
  style
     [
       ("text-align", "center")
     ,  ("margin", "auto") 
     ]

leftPaneStyle : Attribute
leftPaneStyle =
  style
     [
        ("float", "left") 
     ,  ("width", "300px")
     
     ]

rightPaneStyle : Attribute
rightPaneStyle =
  style
     [
        ("float", "left")
     ]

{- gather together all the button attributes -}
buttonAttributes : Bool -> Signal.Address Action -> Action -> List Attribute
buttonAttributes isEnabled address action =
  hoverButton isEnabled ++
    [ bStyle isEnabled
    , onClick address action
    , disabled (not isEnabled)
    ] 

{- style a button -}
bStyle : Bool -> Attribute
bStyle enabled = 
  let
    basecss =
      [
        ("border", "none")
      , ("padding", "2px 10px")
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
      -- , ("margin", "5px 5px 5px 5px")
      , ("margin", "10px 0px 10px 25px")
      , ("font", "100% \"Trebuchet MS\", Verdana, sans-serif")
      , ("-webkit-transition-duration", "0.2s")
      , ("-moz-transition-duration", "0.2s")
      , ("transition-duration", "0.2s")
     ]
    colour =
      if enabled then
        [ ("background-color", "#67d665")
        , ("background", "-webkit-gradient(linear, left top, left bottom, from(#3e9c5f), to(#67d665))")
        , ("background", "-webkit-linear-gradient(top, #3e9c5f, #67d665)")
        , ("background", "-moz-linear-gradient(top, #3e9c5f, #67d665)")
        , ("background", "-ms-linear-gradient(top, #3e9c5f, #67d665)")
        , ("background", "-o-linear-gradient(top, #3e9c5f, #67d665)")
        , ("color", "black")
        ]  
      else
        [ ("background-color", "#7D7C7C")
        , ("color", "grey")
        ]
  in
    style (basecss ++ colour)

{- hover over a button -}
hoverButton : Bool -> List Attribute
hoverButton enabled =      
  if enabled then
     hover [("background-color","#67d665","#669966")]
  else
    []
  

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

errorHighlightStyle : Attribute
errorHighlightStyle =
  style
    [ ("color", "red")
    ]

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

-- key signatures
key : PitchClass -> Mode -> KeySignature
key pc m = { pitchClass = pc, accidental = Nothing, mode = m }

sharpKey : PitchClass -> Mode -> KeySignature
sharpKey pc m = { pitchClass = pc, accidental = Just Sharp, mode = m }

flatKey : PitchClass -> Mode -> KeySignature
flatKey pc m = { pitchClass = pc, accidental = Just Flat, mode = m }









