module AbcPlayer exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)


--import Task exposing (..)

import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, mapError)
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
        { init = ( init, Cmd.none ), update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { fontsLoaded : Bool
    , performance : Result String Performance
    }


init : Model
init =
    { fontsLoaded = False
    , performance = Err "not started"
    }



-- UPDATE


type Msg
    = NoOp
    | RequestLoadFonts String
    | LoadFile String
    | FontsLoaded Bool
    | RawAbc (Result Error String)
    | Abc (Result String Performance)
    | Play


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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

        RawAbc abc ->
            update (Abc (parseLoadedFile abc)) model

        Abc result ->
            ( { model | performance = result }, Cmd.none )

        Play ->
            let
                notes =
                    makeMIDINotes model.performance
            in
                ( model
                , requestPlayNoteSequence notes
                )



{- load an ABC file -}


loadAbc : String -> Cmd Msg
loadAbc url =
    Http.send RawAbc (getString url)


parseLoadedFile : Result Error String -> Result String Performance
parseLoadedFile s =
    case s of
        Ok text ->
            text
                |> parse
                |> melodyFromAbcResult
                |> mapError parseError
                |> toPerformance

        Err e ->
            Err (toString e)



-- VIEW


viewPerformanceResult : Result String Performance -> String
viewPerformanceResult mr =
    case mr of
        Ok res ->
            "OK: " ++ (toString res)

        Err errs ->
            "Fail: " ++ (toString errs)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (RequestLoadFonts "assets/soundfonts") ] [ text "load fonts" ]
        , button [ onClick (LoadFile "abc/lillasystern.abc") ] [ text "load abc file" ]
        , div [] [ text ("parsed abc result: " ++ (viewPerformanceResult model.performance)) ]
        , button [ onClick Play ] [ text "play" ]
        ]
