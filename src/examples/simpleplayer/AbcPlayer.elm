module AbcPlayer where

import Effects exposing (Effects, task)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import Dict exposing (Dict)
import SoundFont exposing (..)
import Abc exposing (..)
import Music.Notation exposing (..)
import AbcPerformance exposing (..)
import Melody exposing (..)
import Notable exposing (..)

-- MODEL
type alias Sound = Task Effects.Never ()
type alias Sounds = List Sound

type alias Model =
    { samples : Dict Int SoundSample
    , loaded : Bool
    , performance : Result String Performance
    }

init : String -> (Model, Effects Action)
init topic =
  ( { 
      samples = Dict.empty, 
      loaded = False, 
      performance = Err "not started"
    }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp   
    | LoadFont (Maybe SoundSample)
    | Abc (Result String Performance )
    | Play

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
               ({ model | loaded = True }, loadAbc "abc/lillasystern.abc" )
               -- ({ model | loaded = True }, loadAbc "abc/tie.abc" )
               -- ({ model | loaded = True }, loadAbc "abc/justnotes.abc" )
            _ -> 
              let pitch = toInt ss.name
              in
                ( { model | samples = Dict.insert pitch ss model.samples }, 
                  Effects.none
                )        

    Abc result ->  ( { model | performance = result }, Effects.none ) 

    Play -> (model, playSounds <| makeSounds model.samples model.performance)   


   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x


{- load an ABC file -}
loadAbc : String -> Effects Action
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
          |> Task.map Abc
          |> Effects.task

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
makeSounds :  Dict Int SoundSample -> Result String Performance -> Sounds 
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


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ 
      div [  ] [ text ("parsed abc result: " ++ (viewPerformanceResult model.performance)) ]
    , button [ onClick address Play ] [ text "play" ]
    ]

-- INPUTS

-- try to load the entire piano soundfont
pianoFonts : Signal (Maybe SoundSample)
pianoFonts = loadSoundFont  "acoustic_grand_piano"

signals : List (Signal Action)
signals = [Signal.map LoadFont pianoFonts]





