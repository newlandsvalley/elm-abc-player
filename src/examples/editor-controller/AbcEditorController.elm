module AbcEditorController exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick, onInput)
import Html.App as Html
import Task exposing (Task, andThen, succeed, sequence, onError)
import List exposing (isEmpty)
import Maybe exposing (Maybe, withDefault)
import String exposing (slice)
import Result exposing (Result, formatError)
import Abc exposing (..)
import Abc.ParseTree exposing (AbcTune, PitchClass (..), Mode (..), Accidental (..), ModifiedKeySignature, KeySignature)
import Abc.Canonical exposing (fromResult, fromTune)
import Music.Notation exposing (getKeySig)
import Music.Transposition exposing (transposeTo)
import Music.Octave exposing (up, down)
import MidiMelody exposing (..)
import MidiPerformance exposing (midiRecordingFromAbc)
import MidiTypes exposing (MidiEvent(..), MidiRecording)
import Midi.Player exposing (Model, Msg, init, update, view, subscriptions)
import FileIO.Ports exposing (..)
import Json.Decode as Json exposing (succeed)
import Debug exposing (..)

{-| Another ABC editor.  It continually parses the ABC as it is entered and flags up errors.
    Buttons are always active, and selecting them stops the playback if the tune is playing. 

    This one translates the ABC to MIDI and uses the Midi.Player module to play it

-}

main =
  Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL

type alias Model =
    {
      abc : String
    , tuneResult : Result ParseError AbcTune
    , player : Midi.Player.Model
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

{-| initialise the model and delegate the initial command to that of the player -}
init : (Model, Cmd Msg)
init =
  let
    recording = Err "not started"
    (player, playerCmd) = Midi.Player.init recording
  in
    { 
      abc = ""
    , tuneResult = Ok emptyTune
    , player = player
    } ! [Cmd.map PlayerMsg playerCmd]


-- UPDATE

type Msg
    = NoOp   
    | Abc String                               -- get the ABC text from the text area
    | Transpose String                         -- transpose the ABC
    | MoveOctave Bool                          -- move the octave (up or down)
    | TuneResult (Result ParseError AbcTune)   -- parsed ABC
    | RequestFileUpload                        -- request an ABC upload
    | FileLoaded (Maybe String)                -- returned loaded ABC
    | PlayerMsg Midi.Player.Msg                -- delegated messages for the player

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )

    Abc s ->  ( { model | abc = s }, checkAbc s )     

    Transpose s -> 
      let
        newmodel = transpose s model
      in
        (newmodel, establishRecording newmodel.tuneResult)

    MoveOctave isUp -> 
      let
        newmodel =
          if isUp then
            moveOctave up model
          else
            moveOctave down model
      in
        (newmodel, establishRecording newmodel.tuneResult)

    TuneResult tr ->  ( { model | tuneResult = tr }, establishRecording tr) 

    RequestFileUpload -> 
       (model, requestLoadFile () )

    FileLoaded maybes ->
      let
         _ = log "elm file input" maybes
      in
       case maybes of
         Just s ->
           ( { model | abc = s }, checkAbc s )
         Nothing ->
           (model, Cmd.none )

    PlayerMsg playerMsg -> 
      let 
        (newPlayer, cmd) = Midi.Player.update playerMsg model.player
      in 
        { model | player = newPlayer } ! [Cmd.map PlayerMsg cmd]

 
returnTuneResult : Result ParseError AbcTune -> Cmd Msg
returnTuneResult r =
  Task.perform (\_ -> NoOp) TuneResult (Task.succeed r)

establishRecording : Result ParseError AbcTune -> Cmd Msg
establishRecording r =
  let
    midiRecording = 
      toMidiRecording r
    -- _ = log "midi recording" midiRecording
    nullTask = Task.succeed (\_ -> ())              
  in
    Task.perform (\_ -> NoOp) 
                 (\_ -> PlayerMsg (Midi.Player.SetRecording midiRecording)) 
                 (nullTask)


terminateLine : String -> String
terminateLine s =
  s ++ "\r\n"

toMidiRecording : Result ParseError AbcTune -> Result String MidiRecording
toMidiRecording r = 
   formatError (\_ -> "some error or other") r
      |> Result.map (midiRecordingFromAbc True) 

{- continually parse the ABC after every key stroke -}
checkAbc : String -> Cmd Msg
checkAbc abc = 
  let 
    terminatedAbc = terminateLine abc
    -- _ = "checking" terminatedAbc
    pr = parse terminatedAbc
  in 
    returnTuneResult (pr)

{- handle the upload request -}
    
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
-- subscription from the FileIO port
fileLoadedSub : Sub Msg
fileLoadedSub  =
  fileLoaded FileLoaded


-- overall subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch 
    [ -- subscription from the MIDI player 
      Sub.map PlayerMsg (Midi.Player.subscriptions model.player)
    , fileLoadedSub
    ]      

-- VIEW

viewError : Model -> Html Msg
viewError m =
  let 
    tuneResult = m.tuneResult
    textRange = 10  -- the range of characters to display around each side of the error position
  in
    case tuneResult of
      Err e -> 
        -- we start off with a dummy error message which is empty
        if (isEmpty e.msgs) then
          text ""
        else
          let
            -- display a prefix of 5 characters before the error (if they're there) and a suffix of 5 after
            startPhrase = Basics.max (e.position - textRange) 0
            errorPrefix = "error: " ++ slice startPhrase e.position m.abc
            startSuffix = Basics.min (e.position + 1) (String.length m.abc)
            endSuffix = Basics.min (e.position + textRange + 1) (String.length m.abc)
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
  if (True) then
    div [ ]
      [  
         h1 [ centreStyle ] [ text "ABC Editor" ]   
      ,  div [ leftPaneStyle ]
           [ 
             span [ leftPanelLabelStyle  ]  [text "Load an ABC file:"]   
           , input [ type' "file"
                   , id "fileinput"   -- FileIO port requires this exact id to be set 
                   , accept ".abc" 
                   -- , property "media_type" (Json.string "text/vnd.abc")
                   -- , on "change" (Json.succeed RequestFileUpload)
                   , onClick RequestFileUpload
                   , inputStyle
                   ] []
           , span [ leftPanelLabelStyle ] [text "Transpose to:"]
           , transpositionMenu model 
           , span [ leftPanelLabelStyle ] [text "Move octave:"]     
           , button ( buttonAttributes True (MoveOctave True))
                       [ text "up" ]   
           , button ( buttonAttributes True (MoveOctave False))
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
 
                 Html.map PlayerMsg (Midi.Player.view model.player) 
 
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
        select [ leftPanelLabelStyle
               , on "change" (Json.map Transpose targetValue)
               ] 
          (transpositionOptions mks)
      Nothing -> 
        select [ leftPanelLabelStyle
               ] 
          [
            option [] [text "unavailable" ]
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


leftPanelLabelStyle : Attribute msg
leftPanelLabelStyle =
  style
    [      
      ("margin-left", "40px")
    , ("margin-top", "40px")
    , ("font-size", "1.2em")
    ]

{- style an input -}
inputStyle : Attribute Msg
inputStyle =
  style
    [
      ("padding", "10px 0")
    , ("font-size", "1em")
    , ("margin-left", "40px")
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

{- gather together all the button attributes 

   In this version of the editor, buttons are enabled all the time.  They stop the tune
   when they are selected if the tune happens to be playing
-}
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











