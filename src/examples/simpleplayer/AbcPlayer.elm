module AbcPlayer exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Html.App as Html
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import SoundFont.Ports exposing (..)
import SoundFont.Types exposing (..)
import Abc exposing (..)
import Music.Notation exposing (..)
import AbcPerformance exposing (..)
import Melody exposing (..)
import Notable exposing (..)

main =
  Html.program
    { init = (init, Cmd.none), update = update, view = view, subscriptions = \_ -> Sub.none }

-- MODEL
type alias Model =
    { 
      fontsLoaded : Bool
    , performance : Result String Performance
    }

init : Model
init =
    {       
      fontsLoaded = False
    , performance = Err "not started"
    }

-- UPDATE

type Msg
    = NoOp   
    | RequestLoadFonts String
    | LoadFile String
    | FontsLoaded Bool
    | Abc (Result String Performance )
    | Play

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )

    RequestLoadFonts dir ->
      ( model
      , requestLoadFonts dir
      )
    LoadFile name ->
      ( model
      , loadAbc name
      )
    FontsLoaded loaded ->
      ( { model | fontsLoaded = loaded }
      , Cmd.none
      )  

    Abc result ->  ( { model | performance = result }, Cmd.none ) 

    Play ->
      let
        notes = makeMIDINotes model.performance
      in
        ( model
        , requestPlayNoteSequence notes 
        ) 
   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x


{- load an ABC file -}
loadAbc : String -> Cmd Msg
loadAbc url = 
      let settings =  { defaultSettings | desiredResponseType  = Just "text/plain; charset=utf-8" }   
        in
          Http.send settings
                          { verb = "GET"
                          , headers = []
                          , url = url
                          , body = empty
                          } 
          |> Task.toResult
          |> Task.map extractResponse
          |> Task.map parseLoadedFile
          |> Task.perform (\_ -> NoOp) Abc

{- inspect the next performance event and generate the appropriate sound command 
   which is done by looking up the sound fonts.  
-}
{-
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
-}
           

{- make the sounds - if we have a performance result from parsing the midi file, convert
   the performance into a list of soundbites (aka Sounds)
-}
{-
makeSounds :  Maybe AudioContext -> Dict Int SoundSample -> Result String Performance -> Sounds 
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
-}

{- play the sounds as a single uninterruptible task -}
{-
playSounds : Sounds -> Effects Action
playSounds sounds = 
      sequence sounds
      |> Task.map (\x -> NoOp)
      |> Effects.task
-}


{- extract the true response, concentrating on 200 statuses - assume other statuses are in error
   (usually 404 not found)
-}
extractResponse : Result RawError Response -> Result String Value
extractResponse result = case result of
    Ok response -> case response.status of
        200 -> Ok response.value
        _ -> Err (toString (response.status) ++ ": " ++ response.statusText)
    Err e -> Err "unexpected http error"

{- cast a String to an Int -}
toInt : String -> Int
toInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0

toPerformance : Result String MelodyLine -> Result String Performance
toPerformance ml = Result.map (fromMelodyLine 0.0) ml

parseLoadedFile : Result String Value -> Result String Performance
parseLoadedFile r = 
  case r of
    Ok text -> case text of
      Text s -> 
        s 
         |> parse 
         |> melodyFromAbcResult 
         |> formatError parseError
         |> toPerformance
      Blob b -> 
        Err "Blob unsupported"
    Err e -> Err e

-- VIEW

viewPerformanceResult : Result String Performance -> String
viewPerformanceResult mr = case mr of
      Ok res -> "OK: " ++ (toString res)
      Err errs -> "Fail: " ++ (toString errs)


view : Model -> Html Msg
view model =
  div []
    [ 
      button [ onClick (RequestLoadFonts "assets/soundfonts") ] [ text "load fonts" ]
    , button [ onClick (LoadFile "abc/lillasystern.abc") ] [ text "load abc file" ]
    , div [  ] [ text ("parsed abc result: " ++ (viewPerformanceResult model.performance)) ]
    , button [ onClick Play ] [ text "play" ]
    ]

-- THE ACTUAL WORK
  
{- make the next MIDI note -}
makeMIDINote : (Float, Notable) -> MidiNote
makeMIDINote ne = 
  let 
    (time, notable) = ne
  in
    case notable of
       -- we've hit a Note
       Note pitch velocity ->
         MidiNote pitch time velocity

{- make the MIDI notes - if we have a performance result from parsing the midi file, convert
   the performance into a list of MidiNote
-}
makeMIDINotes :  Result String Performance -> MidiNotes
makeMIDINotes perfResult = 
  case perfResult of
    Ok perf ->
      List.map makeMIDINote perf
    Err err ->
      []



