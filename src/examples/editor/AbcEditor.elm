module AbcEditor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick, onInput)
import Html.App as Html
import Task exposing (Task, andThen, succeed, sequence, onError)
import Process exposing (sleep)
import List exposing (reverse, isEmpty)
import Maybe exposing (Maybe, withDefault)
import String exposing (toInt, slice)
import Result exposing (Result, formatError)
import Array exposing (Array, get)
import SoundFont.Ports exposing (..)
import SoundFont.Types exposing (..)
import Abc exposing (..)
import AbcPerformance exposing (melodyFromAbcResult)
import Abc.ParseTree exposing (AbcTune, PitchClass (..), Mode (..), Accidental (..), ModifiedKeySignature, KeySignature)
import Abc.Canonical exposing (fromResult, fromTune)
import Music.Notation exposing (getKeySig)
import Music.Transposition exposing (transposeTo)
import Music.Octave exposing (up, down)
import Melody exposing (..)
import Notable exposing (..)
import MidiNotes exposing (..)
import Json.Decode as Json exposing (succeed)
import Debug exposing (..)

{-| An ABC editor.  It continually parses the ABC as it is entered and flags up errors.
    If the (checked) tune contains a key signature, then transposition options will be shown. 

-}

main =
  Html.program
    { init = (init, requestLoadFonts "assets/soundfonts"), update = update, view = view, subscriptions = subscriptions }

-- MODEL

type alias Model =
    {
      fontsLoaded : Bool
    , playing : Bool
    , abc : String
    , tuneResult : Result ParseError AbcTune
    , duration : Float -- the tune duration in seconds
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

init : Model
init =
  {       
    fontsLoaded = False
  , playing = False
  , abc = ""
  , tuneResult = Ok emptyTune
  , duration = 0.0
  }

-- UPDATE

type Msg
    = NoOp   
    | FontsLoaded Bool
    | Abc String
    | Play                -- request that a tune plays
    | PlayStarted Bool    -- response from the player that it's started
    | PlayCompleted       -- the play has completed (we compute the time ourselves)
    | Transpose String
    | MoveOctave Bool
    | TuneResult (Result ParseError AbcTune)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )

    FontsLoaded loaded ->
      ( { model | fontsLoaded = loaded }
      , Cmd.none
      )  

    Abc s ->  ( { model | abc = s }, checkAbc s )     

    Play -> playAbc model   

    PlayStarted _ -> (model, (suspend model.duration) )

    PlayCompleted -> ( { model | playing = False }, Cmd.none)   

    Transpose s -> (transpose s model, Cmd.none )

    MoveOctave isUp -> 
      if isUp then 
        (moveOctave up model, Cmd.none )
      else
        (moveOctave down model, Cmd.none )

    TuneResult tr ->  ( { model | tuneResult = tr }, Cmd.none) 

{- a different attempt at checking if buttons are enabled -}
areButtonsEnabled : Model -> Bool
areButtonsEnabled m =
  case m.tuneResult of
    Ok _ -> 
      not (m.playing)
    Err _ -> False

{- sleep for a number of seconds -}
suspend : Float -> Cmd Msg
suspend secs =
  let
    _ = log "suspend time" secs
    time = secs * 1000
  in 
    Process.sleep time
      |> Task.perform (\_ -> NoOp) (\_ -> PlayCompleted)
    
returnTuneResult : Result ParseError AbcTune -> Cmd Msg
returnTuneResult r =
  Task.perform (\_ -> NoOp) TuneResult (Task.succeed r)
    
terminateLine : String -> String
terminateLine s =
  s ++ "\r\n" 

{- cast a String to an Int -}
toInt : String -> Int
toInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0

{- continually parse the ABC after every key stroke -}
checkAbc : String -> Cmd Msg
checkAbc abc = 
  let 
    terminatedAbc = terminateLine abc
    -- _ = "checking" terminatedAbc
    pr = parse terminatedAbc
  in 
    returnTuneResult (pr)
    
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
      

-- VIEW

viewError : Model -> Html Msg
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

view : Model -> Html Msg
view model =
  if (model.fontsLoaded) then
    div [ ]
      [  
         h1 [ centreStyle ] [ text "ABC Editor" ]   
      ,  div [ leftPaneStyle ]
           [ span [ leftPanelWidgetStyle ] [text "Transpose to:"]
           , transpositionMenu model 
           , span [ leftPanelWidgetStyle ] [text "Move octave:"]     
           , button ( buttonAttributes (areButtonsEnabled model) (MoveOctave True))
                       [ text "up" ]   
           , button ( buttonAttributes (areButtonsEnabled model) (MoveOctave False))
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
               , onInput Abc 
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
 
                 button ( buttonAttributes (areButtonsEnabled model) Play)
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
transpositionMenu : Model -> Html Msg
transpositionMenu m =
  let 
    mKeySig =
      case
        m.tuneResult of
          Ok tune -> defaultToC (getKeySig tune)
          _ -> Nothing
  in
    case mKeySig of
      Just mks ->
        select [ leftPanelWidgetStyle
               , (disabled m.playing)
               , on "change" (Json.map Transpose targetValue)
               ] 
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
transpositionOptions : ModifiedKeySignature -> List (Html Msg)
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


{- return a (selected true) attribute if the pattern key signature matches the target -}
selectedKey : KeySignature -> KeySignature -> Attribute Msg
selectedKey target pattern =
  let
    isMatched = (target.pitchClass == pattern.pitchClass) && (target.accidental == pattern.accidental)
  in
    selected isMatched

{- display a key signature as text -}
displayKeySig : KeySignature -> Html Msg
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
instructionStyle : Attribute msg
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

leftPanelWidgetStyle : Attribute msg
leftPanelWidgetStyle =
  style
    [      
      ("margin-left", "40px")
    , ("margin-top", "40px")
    , ("font-size", "1.2em")
    ]

{- style a centered component -}    
centreStyle : Attribute msg
centreStyle =
  style
     [
       ("text-align", "center")
     ,  ("margin", "auto") 
     ]

leftPaneStyle : Attribute msg
leftPaneStyle =
  style
     [
        ("float", "left") 
     ,  ("width", "350px")
     
     ]

rightPaneStyle : Attribute msg
rightPaneStyle =
  style
     [
        ("float", "left")
     ]

{- gather together all the button attributes -}
buttonAttributes : Bool -> Msg -> List (Attribute Msg)
buttonAttributes isEnabled msg =
    [ class "hoverable"
    , bStyle isEnabled
    , onClick msg
    , disabled (not isEnabled)
    ] 

{- style a button 
   Note: all button styling is deferred to the external css (which implements hover)
         except for when the button is greyed out when it is disabled
-}
bStyle : Bool -> Attribute msg
bStyle enabled = 
  let
    colour =
      if enabled then
        [ 
        ]  
      else
        [ ("background-color", "lightgray")
        , ("color", "darkgrey")
        ]
  in
    style (colour) 

{- style a fieldset -}
fieldsetStyle : Attribute msg
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

errorHighlightStyle : Attribute msg
errorHighlightStyle =
  style
    [ ("color", "red")
    ]


-- key signatures
key : PitchClass -> Mode -> KeySignature
key pc m = { pitchClass = pc, accidental = Nothing, mode = m }

sharpKey : PitchClass -> Mode -> KeySignature
sharpKey pc m = { pitchClass = pc, accidental = Just Sharp, mode = m }

flatKey : PitchClass -> Mode -> KeySignature
flatKey pc m = { pitchClass = pc, accidental = Just Flat, mode = m }

cMajor : ModifiedKeySignature
cMajor = ({ pitchClass = C, accidental = Nothing, mode = Major }, [])

{- if there's no key signature in a properly parsed tune then default to C -}
defaultToC : Maybe ModifiedKeySignature -> Maybe ModifiedKeySignature
defaultToC mks =
  case mks of
    Just ks -> mks
    _ -> Just cMajor











