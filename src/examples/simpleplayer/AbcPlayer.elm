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
import MidiNotes exposing (..)

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




